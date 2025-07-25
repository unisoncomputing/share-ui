import { test, expect } from "@playwright/test";
import * as API from "./TestHelpers/Api";
import * as Page from "./TestHelpers/Page";
import { notification } from "./TestHelpers/Data";

test.describe("when *NOT* signed in", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "NOT_SIGNED_IN");
    await API.getNotificationsHub(page, "@alice");
  });

  test("results in a 404 page", async ({ page }) => {
    const response = await Page.goto(page, "/notifications");
    expect(response?.status()).toBeLessThan(400);

    await expect(page.getByText("Page not found")).toBeVisible();
  });
});

test.describe("when signed in", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "@alice");
    await API.getNotificationsHub(page, "@alice");
  });

  test("can view the notifications page", async ({ page }) => {
    const response = await Page.goto(page, "/notifications");
    expect(response?.status()).toBeLessThan(400);

    await expect(page.getByText("Notifications")).toBeVisible();
  });

  test("can view notifications", async ({ page }) => {
    await Page.goto(page, "/notifications");
    await expect(page.locator(".notification-row")).toHaveCount(7);
  });

  test("can mark a notification as read", async ({ page }) => {
    await API.getNotificationsHub(page, "@alice", {
      items: [
        notification({ status: "unread" }),
        notification({ status: "unread" }),
        notification({ status: "unread" }),
        notification({ status: "unread" }),
      ],
    });

    await Page.goto(page, "/notifications");

    await expect(page.locator(".notification-row")).toHaveCount(4);
    await page.locator(".notification-row:first-child input").click();
    await expect(Page.button(page, "Mark as read")).toBeVisible();

    await API.patchNotificationsHub(page, "@alice");

    await Page.button(page, "Mark as read").click();

    await expect(
      page.locator(".notification-row:not(.notification-row_unread)"),
    ).toHaveCount(1);
    await expect(
      page.locator(".notification-row.notification-row_unread"),
    ).toHaveCount(3);
  });
});
