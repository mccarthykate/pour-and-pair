// Runs inside a traderjoes.com page open in a browser, during a refresh someone starts
// by hand. Uses the same product search the website uses.
// args: { terms: [search words], names: [exact product names to confirm] }
// Returns { items: [...], names: { name: item | null }, failed: n }
(async (args) => {
  const query = `query S($search:String,$pageSize:Int,$currentPage:Int,$storeCode:String,$availability:String,$published:String){
    products(search:$search,filter:{store_code:{eq:$storeCode},published:{eq:$published},availability:{match:$availability}},pageSize:$pageSize,currentPage:$currentPage){
      items{sku item_title sales_size sales_uom_description retail_price category_hierarchy{name}} total_count}}`;
  const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
  let failed = 0;
  const search = async (term) => {
    const out = [];
    let page = 1, total = 1;
    while (out.length < total && page <= 6) {
      let json = null;
      for (let attempt = 0; attempt < 3 && !json; attempt++) {
        try {
          const r = await fetch('/api/graphql', {
            method: 'POST', headers: { 'content-type': 'application/json' },
            body: JSON.stringify({ operationName: 'S', query,
              variables: { search: term, pageSize: 100, currentPage: page, storeCode: 'TJ', availability: '1', published: '1' } })
          });
          if (r.ok) json = await r.json();
        } catch (e) { /* retry */ }
        if (!json) await sleep(1500);
      }
      if (!json || !json.data || !json.data.products) { failed++; break; }
      const items = json.data.products.items || [];
      total = json.data.products.total_count || 0;
      out.push(...items);
      if (!items.length) break;
      page++;
      await sleep(200);
    }
    return out;
  };
  const clean = (x) => ({
    sku: x.sku, name: (x.item_title || '').replace(/\s+/g, ' ').trim(), price: x.retail_price,
    size: x.sales_size, uom: x.sales_uom_description, category: (x.category_hierarchy || []).map((c) => c.name).join(' > ')
  });
  const bySku = new Map();
  for (const t of args.terms) {
    for (const x of await search(t)) bySku.set(x.sku, clean(x));
    await sleep(150);
  }
  const norm = (s) => s.replace(/\s+/g, ' ').trim().toLowerCase();
  const known = new Map([...bySku.values()].map((x) => [norm(x.name), x]));
  const names = {};
  for (const n of args.names) {
    let hit = known.get(norm(n));
    if (!hit) {
      const found = (await search(n)).map(clean);
      found.forEach((x) => bySku.set(x.sku, x));
      hit = found.find((x) => norm(x.name) === norm(n)) || null;
      await sleep(150);
    }
    names[n] = hit;
  }
  return { items: [...bySku.values()], names, failed };
})
