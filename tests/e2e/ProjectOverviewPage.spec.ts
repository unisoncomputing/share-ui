import { test, expect } from "@playwright/test";

import { navItem, button } from "./TestHelpers/Page";
import * as API from "./TestHelpers/Api";

test.beforeEach(async ({ page }) => {
  await API.getWebsiteFeed(page);
});

test.describe("without being signed in", () => {
  test.beforeEach(async ({ page }) => {
    await API.getProjectReadme(page, "@unison/base");
    await API.getProjectDependencies(page, "@unison/base", "3.33.0");
    await API.getAccount(page, "NOT_SIGNED_IN");
  });

  test("can view public project", async ({ page, isMobile }) => {
    await API.getProject(page, "@unison/base");

    const response = await page.goto("http://localhost:1234/@unison/base");
    expect(response?.status()).toBeLessThan(400);

    const projectName = page.locator(".page-header").getByText("@unison/base");
    await expect(projectName).toHaveClass("project-name");

    await expect(button(page, "Browse Project Code")).toBeVisible();
    await expect(button(page, "Edit summary")).not.toBeVisible();

    // Nav is hidden on mobile
    if (!isMobile) {
      await expect(navItem(page, "Code")).toBeVisible();
      await expect(navItem(page, "Tickets")).toBeVisible();
      await expect(navItem(page, "Contributions")).toBeVisible();
      await expect(navItem(page, "Settings")).not.toBeVisible();
    }
  });

  test("can *not* view a private project`", async ({ page, isMobile }) => {
    await API.getProject_(page, "@bob/private-project", { status: 404 });

    const response = await page.goto(
      "http://localhost:1234/@bob/private-project",
    );
    expect(response?.status()).toBeLessThan(400);

    // await expect(button(page, "Browse Project Code")).not.toBeVisible();
    await expect(
      page.getByText("Couldn't find @bob/private-project"),
    ).toBeVisible();

    // Nav is hidden on mobile
    if (!isMobile) {
      await expect(navItem(page, "Code")).not.toBeVisible();
      await expect(navItem(page, "Tickets")).not.toBeVisible();
      await expect(navItem(page, "Contributions")).not.toBeVisible();
      await expect(navItem(page, "Settings")).not.toBeVisible();
    }
  });
});

test.describe("while signed in", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "@alice");
    await API.getProjectReadme(page, "@bob/private-project");
    await API.getProjectDependencies(page, "@unison/base", "3.33.0");
  });

  test.describe("with an another user's private project and `project:view` permission", () => {
    test("can view, but *not* edit summary or see settings", async ({
      page,
      isMobile,
    }) => {
      await API.getProject(page, "@bob/private-project", {
        visibility: "private",
        permissions: ["project:view"],
      });

      const response = await page.goto(
        "http://localhost:1234/@bob/private-project",
      );
      expect(response?.status()).toBeLessThan(400);

      // Nav is hidden on mobile
      if (!isMobile) {
        await expect(button(page, "Edit summary")).not.toBeVisible();
        await expect(navItem(page, "Code")).toBeVisible();
        await expect(navItem(page, "Tickets")).toBeVisible();
        await expect(navItem(page, "Contributions")).toBeVisible();
        await expect(navItem(page, "Settings")).not.toBeVisible();
      }
    });
  });

  test.describe("with an another user's private project and `project:maintain` permission", () => {
    test("can view and edit summary, but not see settings", async ({
      page,
      isMobile,
    }) => {
      await API.getProject(page, "@bob/private-project", {
        visibility: "private",
        permissions: ["project:view", "project:maintain"],
      });

      const response = await page.goto(
        "http://localhost:1234/@bob/private-project",
      );
      expect(response?.status()).toBeLessThan(400);

      // Nav is hidden on mobile
      if (!isMobile) {
        await expect(button(page, "Edit summary")).toBeVisible();
        await expect(navItem(page, "Code")).toBeVisible();
        await expect(navItem(page, "Tickets")).toBeVisible();
        await expect(navItem(page, "Contributions")).toBeVisible();
        await expect(navItem(page, "Settings")).not.toBeVisible();
      }
    });
  });

  test.describe("with an another user's private project and `project:manage` permission", () => {
    test("can view and edit summary, but not see settings", async ({
      page,
      isMobile,
    }) => {
      await API.getProject(page, "@bob/private-project", {
        visibility: "private",
        permissions: ["project:view", "project:maintain", "project:manage"],
      });

      const response = await page.goto(
        "http://localhost:1234/@bob/private-project",
      );
      expect(response?.status()).toBeLessThan(400);

      // Nav is hidden on mobile
      if (!isMobile) {
        await expect(button(page, "Edit summary")).toBeVisible();
        await expect(navItem(page, "Code")).toBeVisible();
        await expect(navItem(page, "Tickets")).toBeVisible();
        await expect(navItem(page, "Contributions")).toBeVisible();
        await expect(navItem(page, "Settings")).toBeVisible();
      }
    });
  });
});
