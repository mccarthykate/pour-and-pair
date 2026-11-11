// Nightly: open traderjoes.com in a hidden Chromium and collect current products.
// Usage: node scripts/fetch_tj.js  (writes data/tj_snapshot.json)
const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright');

const root = path.join(__dirname, '..');
const terms = JSON.parse(fs.readFileSync(path.join(__dirname, 'search_terms.json'), 'utf8'));
const csvNames = (file) => {
  const lines = fs.readFileSync(path.join(root, 'data', file), 'utf8').trim().split('\n').slice(1);
  // the name is always the 2nd column; values are quoted by write.csv
  return lines.map((l) => (l.match(/^"[^"]*","((?:[^"]|"")*)"/) || [])[1]).filter(Boolean).map((s) => s.replace(/""/g, '"'));
};
const names = [...csvNames('wines.csv'), ...csvNames('cheeses.csv'), ...csvNames('addons.csv')];

(async () => {
  const browser = await chromium.launch();
  const context = await browser.newContext({
    userAgent: 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36',
    locale: 'en-US', viewport: { width: 1280, height: 900 }
  });
  const page = await context.newPage();
  const res = await page.goto('https://www.traderjoes.com/home/search?q=cheese&global=yes', { waitUntil: 'domcontentloaded', timeout: 60000 });
  console.log('Trader Joe\'s page status:', res && res.status());
  await page.waitForTimeout(4000);
  const code = fs.readFileSync(path.join(__dirname, 'tj_page.js'), 'utf8');
  const result = await page.evaluate(`(${code})(${JSON.stringify({ terms: [].concat(...Object.values(terms)), names })})`);
  await browser.close();
  result.fetched_at = new Date().toISOString();
  fs.writeFileSync(path.join(root, 'data', 'tj_snapshot.json'), JSON.stringify(result, null, 1));
  const found = Object.values(result.names).filter(Boolean).length;
  console.log(`Found ${result.items.length} products; confirmed ${found} of ${names.length} catalog items; ${result.failed} failed searches.`);
})().catch((e) => { console.error(e); process.exit(1); });
