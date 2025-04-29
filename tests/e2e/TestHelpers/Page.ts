import { Page } from "@playwright/test";

function button(page: Page, label: string) {
  return page.locator(".button").getByText(label);
}

function navItem(page: Page, label: string) {
  return page.locator(".min-md .nav-item_full-label").getByText(label);
}

export { button, navItem };
