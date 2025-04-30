import { test, expect } from "@playwright/test";
import * as API from "./TestHelpers/Api";
import { userHandle } from "./TestHelpers/Data";

test.beforeEach(async ({ page }) => {
  await API.getWebsiteFeed(page);
});

test.describe("without being signed in", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "NOT_SIGNED_IN");
  });

  test("can view the FinishSignupPage", async ({ page }) => {
    const response = await page.goto(
      `http://localhost:1234/finish-signup?conflictingHandle=${userHandle()}`,
    );
    expect(response?.status()).toBeLessThan(400);

    await expect(page.getByText("Almost there...")).toBeVisible();
  });
});

test.describe("when signed in", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "@alice");
  });

  test("can *NOT* view the FinishSignupagePage", async ({ page }) => {
    const response = await page.goto(
      `http://localhost:1234/finish-signup?conflictingHandle=${userHandle()}`,
    );
    expect(response?.status()).toBeLessThan(400);

    await expect(page.getByText("Almost there...")).not.toBeVisible();
    await expect(page.getByText("Page not Found")).toBeVisible();
  });
});
