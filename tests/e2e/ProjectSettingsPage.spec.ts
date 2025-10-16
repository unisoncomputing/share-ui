import { test, expect } from "@playwright/test";
import { button } from "./TestHelpers/Page";
import * as API from "./TestHelpers/Api";

const projectRef = "@alice/html";

test.beforeEach(async ({ page }) => {
  await API.getWebsiteFeed(page);
});

test.describe("without being signed in", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "NOT_SIGNED_IN");
    await page.goto(`http://localhost:1234/${projectRef}/settings`);
  });

  test("can *NOT* see the project settings page", async ({ page }) => {
    await expect(page.getByText("Project Settings")).not.toBeVisible();
  });
});

test.describe("with the project:manage permission", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "@alice");
    await API.getUserProfile(page, "@alice");
    await API.getProjectRoleAssignments(page, projectRef);
    await API.getProjectWebhooks(page, projectRef);
    await API.getProject(page, projectRef, {
      visibility: "public",
      permissions: ["project:manage"],
    });
    await page.goto(`http://localhost:1234/${projectRef}/settings`);
  });

  test("The user can see the project settings page", async ({ page }) => {
    await expect(page.getByText("Project Settings")).toBeVisible();
  });

  test("The user can see the the collaborators section", async ({ page }) => {
    await expect(page.locator(".collaborator")).toHaveCount(3);
  });

  test("The user can see the the visibility section", async ({ page }) => {
    await expect(page.getByText("Project Visibility")).toBeVisible();
  });

  test.describe("webhooks", () => {
    test("The user can see the webhooks section", async ({ page }) => {
      await expect(page.locator(".webhook")).toHaveCount(3);
      await expect(button(page, "Add a webhook")).toBeVisible();
    });

    test("The user can see the add webhook modal", async ({ page }) => {
      await button(page, "Add a webhook").click();
      await expect(page.locator(".modal")).toBeVisible();
    });
  });
});
