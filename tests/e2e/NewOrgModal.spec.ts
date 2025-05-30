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

  test("can *NOT* see the 'New Org' button", async ({ page, isMobile }) => {
    const response = await page.goto("http://localhost:1234");
    expect(response?.status()).toBeLessThan(400);
    await expect(page).toHaveScreenshot("no-new-org-button.png", { maxDiffPixels: 100 });

    if (isMobile) {
      await expect(
        page.locator(".signed-in-nav_mobile .button").getByText("New Org"),
      ).toBeVisible();
    } else {
      await expect(
        page.locator(".signed-in-nav_desktop .button").getByText("New Org"),
      ).toBeVisible();
    }
  });
});

test.describe("while being signed in", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "@alice", { isSuperadmin: true });
  });

  test("can see the 'New Org' button", async ({ page, isMobile }) => {
    const response = await page.goto("http://localhost:1234");
    expect(response?.status()).toBeLessThan(400);
    await expect(page).toHaveScreenshot("new-org-button.png", { maxDiffPixels: 100 });

    if (isMobile) {
      await expect(
        page.locator(".signed-in-nav_mobile .button").getByText("New Org"),
      ).toBeVisible();
    } else {
      await expect(
        page.locator(".signed-in-nav_desktop .button").getByText("New Org"),
      ).toBeVisible();
    }
  });
});
