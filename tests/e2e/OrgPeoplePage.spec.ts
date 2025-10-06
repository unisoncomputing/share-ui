import { test, expect } from "@playwright/test";
import { navItem, button } from "./TestHelpers/Page";
import * as API from "./TestHelpers/Api";

test.beforeEach(async ({ page }) => {
  await API.getWebsiteFeed(page);
});

test.describe("without being signed in", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "NOT_SIGNED_IN");
  });

  test("can *NOT* view the org people page", async ({ page }) => {
    const orgHandle = "@unison";
    await API.getOrgProfile(page, orgHandle);
    await API.getOrgRoleMembers_(page, orgHandle, { status: 403 });

    const response = await page.goto(
      `http://localhost:1234/${orgHandle}/p/people`,
    );
    expect(response?.status()).toBeLessThan(400);

    const handle = page.locator(".page-header").getByText("@unison");
    await expect(handle).toBeVisible();

    const title = page.getByText("You're not authorized to view this page");
    await expect(title).toBeVisible();
  });
});

test.describe("without org:manage permission", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "@alice");
  });

  test("can *NOT* view the org people page", async ({ page }) => {
    const orgHandle = "@unison";
    await API.getOrgProfile(page, orgHandle, { permissions: ["org:view"] });
    await API.getOrgRoleMembers_(page, orgHandle, { status: 403 });

    const response = await page.goto(
      `http://localhost:1234/${orgHandle}/p/people`,
    );
    expect(response?.status()).toBeLessThan(400);

    const handle = page.locator(".page-header").getByText("@unison");
    await expect(handle).toBeVisible();

    const title = page.getByText("You're not authorized to view this page");
    await expect(title).toBeVisible();
  });
});

test.describe("with org:manage permission", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "@alice");
  });

  test("can view the org people page", async ({ page }) => {
    const orgHandle = "@unison";
    await API.getOrgProfile(page, orgHandle, { permissions: ["org:manage"] });
    await API.getOrgRoleMembers(page, orgHandle);

    const response = await page.goto(
      `http://localhost:1234/${orgHandle}/p/people`,
    );
    expect(response?.status()).toBeLessThan(400);

    const handle = page.locator(".page-header").getByText("@unison");
    await expect(handle).toBeVisible();

    const projects = page.locator(".page-content .profile-snippet");
    await expect(projects).toHaveCount(3);

    await expect(navItem(page, "People")).toBeVisible();
    await expect(button(page, "Add an organization member")).toBeVisible();
  });
});
