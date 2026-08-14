// e2e.mjs — end-to-end test of the tldraw web canvas against a mock OrgPad
// server, driven through a real headless browser (Playwright/Chromium).
//
// It boots a throwaway HTTP server that speaks the OrgPad receiver protocol
// (/canvas, /pair, /session, /result), loads the real web/canvas.html in
// Chromium, and drives the whole flow the way an iPad would:
//   pairing -> POST /pair -> waiting -> GET /session (204 then 200) ->
//   drawing -> draw a stroke with the pointer -> Done -> POST /result,
// then asserts the uploaded body carries a PNG and a valid tldraw snapshot.
// A second pass feeds that snapshot back as an "edit" session and asserts the
// shapes are restored.
//
// Requires Playwright + Chromium. If Playwright isn't installed the test SKIPS
// (exit 0) so a fresh checkout's `make web` doesn't hard-fail; install it with
//   npm i -D playwright && npx playwright install chromium
// (or run with NODE_PATH pointing at an existing Playwright install).
//
// Run:  node e2e.mjs
import { readFileSync } from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";
import { dirname, join } from "node:path";
import http from "node:http";

const __dirname = dirname(fileURLToPath(import.meta.url));

let chromium;
async function loadPlaywright() {
  const cands = [];
  if (process.env.ORGPAD_PLAYWRIGHT) cands.push(pathToFileURL(process.env.ORGPAD_PLAYWRIGHT).href); // explicit module path
  cands.push("playwright");                                                     // normal resolution
  for (const c of cands) { try { const m = await import(c); if (m.chromium || (m.default && m.default.chromium)) return m.chromium || m.default.chromium; } catch {} }
  return null;
}
chromium = await loadPlaywright();
if (!chromium) {
  console.log("SKIP e2e.mjs — Playwright not installed (npm i -D playwright && npx playwright install chromium).");
  process.exit(0);
}

const HTML = readFileSync(join(__dirname, "canvas.html"), "utf-8");
function pageWithConfig(cfg) {
  const inject = `<script>window.ORGPAD_CONFIG=${JSON.stringify(cfg)}</script>`;
  return HTML.includes("</head>") ? HTML.replace("</head>", inject + "</head>") : inject + HTML;
}

function readBody(req) {
  return new Promise((res) => { let b = ""; req.on("data", (c) => (b += c)); req.on("end", () => res(b)); });
}

// ---- mock server state ----
const server = {
  config: {},              // config injected into /canvas
  sessionPayload: null,    // what GET /session returns on the "ready" call
  sessionCallsUntilReady: 1,
  sessionCalls: 0,
  resultBody: null,
  pairBody: null,
};

const httpServer = http.createServer(async (req, res) => {
  const url = req.url.split("?")[0];
  if (url === "/canvas") {
    res.writeHead(200, { "Content-Type": "text/html; charset=utf-8", "Cache-Control": "no-store" });
    res.end(pageWithConfig(server.config));
  } else if (url === "/pair" && req.method === "POST") {
    server.pairBody = JSON.parse((await readBody(req)) || "{}");
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ token: "tok-e2e" }));
  } else if (url === "/session" && req.method === "GET") {
    server.sessionCalls++;
    if (server.sessionPayload && server.sessionCalls >= server.sessionCallsUntilReady) {
      res.writeHead(200, { "Content-Type": "application/json" });
      res.end(JSON.stringify(server.sessionPayload));
    } else {
      res.writeHead(204); res.end();
    }
  } else if (url === "/result" && req.method === "POST") {
    server.resultBody = JSON.parse((await readBody(req)) || "{}");
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ ok: true }));
  } else { res.writeHead(404); res.end("nope"); }
});

await new Promise((r) => httpServer.listen(0, "127.0.0.1", r));
const PORT = httpServer.address().port;
const ORIGIN = `http://127.0.0.1:${PORT}`;

let passed = 0;
function t(name, cond) { if (cond) { passed++; console.log("  ok  " + name); } else { console.error("FAIL  " + name); process.exitCode = 1; } }

const browser = await chromium.launch();
const errors = [];
async function newPage() {
  const ctx = await browser.newContext({ viewport: { width: 1180, height: 820 }, deviceScaleFactor: 2 });
  const page = await ctx.newPage();
  page.on("pageerror", (e) => errors.push("pageerror: " + e.message));
  page.on("console", (m) => { if (m.type() === "error") errors.push("console: " + m.text()); });
  return page;
}
async function drawStroke(page, pts) {
  await page.mouse.move(pts[0][0], pts[0][1]); await page.mouse.down();
  for (const [x, y] of pts.slice(1)) await page.mouse.move(x, y, { steps: 6 });
  await page.mouse.up();
}
const editorReady = (page) => page.waitForFunction(() => window.__orgpad && window.__orgpad.editor, null, { timeout: 30000 });

// ============================ Scenario A: new figure ============================
console.log("== receiver: pair -> wait -> draw -> Done ==");
// Mirror the exact field set org-pad--canvas-config injects (absolute resultUrl
// + a relative result_path alias) so we exercise the real precedence.
server.config = { token_header: "X-OrgPad-Token", pair_path: "/pair", session_path: "/session",
                  resultUrl: ORIGIN + "/result", result_path: "/result", cancel_path: "/cancel", format: "web" };
// custom color background exercises the exportResult -> bakeBackground composite path
server.sessionPayload = { session_id: "s-new", mode: "new", name: "", background: "#204030", format: "web" };
server.sessionCallsUntilReady = 2;   // one 204, then the 200
server.sessionCalls = 0; server.resultBody = null; server.pairBody = null;

let snapshotB64 = null;
{
  const page = await newPage();
  await page.goto(ORIGIN + "/canvas", { waitUntil: "load" });
  // pairing screen
  await page.waitForSelector("#screenPairing:not([hidden])", { timeout: 30000 });
  t("boots to the pairing screen (no token)", true);
  await page.fill("#pairCode", "123456");
  await page.click("#btnPair");
  // it should reach the drawing surface once /session returns 200
  await editorReady(page);
  await page.waitForSelector("#btnDone", { timeout: 30000 });   // Done lives in tldraw's SharePanel now
  // Wait until the drawing surface is actually active (all overlays hidden) —
  // tldraw + #btnDone exist from boot behind the pairing/waiting overlays, so
  // gate the draw on the overlays being gone, not just on the button existing.
  await page.waitForFunction(() =>
    ["screenPairing", "screenWaiting", "screenDone", "screenLoading", "screenError"]
      .every((id) => { const e = document.getElementById(id); return !e || e.hidden; }),
    null, { timeout: 30000 });
  t("POST /pair sent the exact {code} body", server.pairBody && server.pairBody.code === "123456");
  await drawStroke(page, [[360, 300], [430, 250], [520, 340], [600, 280]]);
  const shapes = await page.evaluate(() => window.__orgpad.editor.getCurrentPageShapeIds().size);
  t("a stroke was drawn on the tldraw surface", shapes >= 1);
  await page.click("#btnDone");
  for (let i = 0; i < 200 && !server.resultBody; i++) await new Promise((r) => setTimeout(r, 50)); // wait for the actual POST
  const rb = server.resultBody;
  t("POST /result received", !!rb);
  t("/result carries session_id + format:web", rb && rb.session_id === "s-new" && rb.format === "web");
  t("/result PNG is non-empty", rb && typeof rb.png === "string" && rb.png.length > 100);
  let snap = null;
  try { snap = JSON.parse(Buffer.from(rb.drawing, "base64").toString("utf-8")); } catch {}
  t("/result drawing decodes to a tldraw snapshot (has .document)", !!(snap && snap.document));
  snapshotB64 = rb.drawing;
  await page.context().close();
}

// ============================ Scenario B: edit restore ============================
console.log("== edit: server sends a snapshot -> shapes restored ==");
{
  const editCfg = {
    token_header: "X-OrgPad-Token", pair_path: "/pair", session_path: "/session",
    result_path: "/result", cancel_path: "/cancel",
    session_id: "s-edit", token: "tok-e2e", mode: "edit", name: "fig-1",
    background: "transparent", drawing: snapshotB64
  };
  server.config = editCfg;
  const page = await newPage();
  await page.goto(ORIGIN + "/canvas", { waitUntil: "load" });
  await editorReady(page);
  await page.waitForTimeout(400);
  const shapes = await page.evaluate(() => window.__orgpad.editor.getCurrentPageShapeIds().size);
  t("edit session restored the saved shapes into tldraw", shapes >= 1);
  await page.context().close();
}

t("no uncaught page errors across the run", errors.length === 0);
if (errors.length) console.error("   errors:\n   " + errors.slice(0, 6).join("\n   "));

await browser.close();
await new Promise((r) => httpServer.close(r));
console.log("\n" + passed + " assertions passed" + (process.exitCode ? " (with failures above)" : ", 0 failed"));
