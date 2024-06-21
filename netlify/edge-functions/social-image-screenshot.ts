import type { Context } from "@netlify/edge-functions";
import puppeteer from "puppeteer";

export default async (request: Request, _context: Context) => {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();

  await page.setViewport({ width: 1200, height: 600 });
  const website_url = "https://share.unison-lang.org";

  await page.goto(website_url, { waitUntil: 'networkidle0' });

  const ss = await page.screenshot();

  return ss;
};
