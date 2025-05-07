import { test, expect } from "@playwright/test";
import * as API from "./TestHelpers/Api";

test.beforeEach(async ({ page }) => {
  await API.getWebsiteFeed(page);
  await API.getCatalog(page);
});

test.describe("without being signed in", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "NOT_SIGNED_IN");
  });

  test("can *NOT* see the 'New Org' button", async ({ page }) => {
    const response = await page.goto("http://localhost:1234");
    expect(response?.status()).toBeLessThan(400);

    await expect(page.locator(".signed-in-nav_desktop .button").getByText("New Org")).not.toBeVisible();
  });
});

test.describe("while being signed in", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "@alice", { isSuperadmin: true });
  });

  test("can see the 'New Org' button", async ({ page }) => {
    const response = await page.goto("http://localhost:1234");
    expect(response?.status()).toBeLessThan(400);

    await expect(page.locator(".signed-in-nav_desktop .button").getByText("New Org")).toBeVisible();
  });
});
