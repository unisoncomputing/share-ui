import { test, expect } from "@playwright/test";
import * as API from "./TestHelpers/Api";

test.beforeEach(async ({ page }) => {
  await API.getWebsiteFeed(page);
});

test("can view a user profile", async ({ page }) => {
  await API.getAccount(page, "@alice");
  await API.getUserProfile(page, "@bob");
  await API.getProjects(page, "@bob");

  const response = await page.goto("http://localhost:1234/@bob");
  expect(response?.status()).toBeLessThan(400);

  const handle = page.locator(".page-header").getByText("@bob");
  await expect(handle).toBeVisible();

  const projects = page.locator(".project-listing");
  await expect(projects).toHaveCount(3);
});

test("can view an org profile", async ({ page }) => {
  await API.getAccount(page, "@alice");
  await API.getOrgProfile(page, "@unison");
  await API.getProjects(page, "@unison");

  const response = await page.goto("http://localhost:1234/@unison");
  expect(response?.status()).toBeLessThan(400);

  const handle = page.locator(".page-header").getByText("@unison");
  await expect(handle).toBeVisible();

  const projects = page.locator(".project-listing");
  await expect(projects).toHaveCount(3);
});
