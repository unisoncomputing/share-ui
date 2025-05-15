import { Page } from "@playwright/test";

async function goto(page: Page, url: string) {
  return page.goto(`http://localhost:1234/${url.replace("/", "")}`);
}

function button(page: Page, label: string) {
  return page.locator(".button").getByText(label);
}

function navItem(page: Page, label: string) {
  return page.locator(".min-md .nav-item_full-label").getByText(label);
}

export { button, navItem, goto };
