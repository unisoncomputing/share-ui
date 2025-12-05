export async function get(url: string, timeout: number = 30000) {
  const c = new AbortController();
  const id = setTimeout(() => c.abort(), timeout);
  const res = await fetch(url, { signal: c.signal });
  clearTimeout(id);
  return res;
}
