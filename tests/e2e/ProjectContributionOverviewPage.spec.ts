import { test, expect } from "@playwright/test";

import { contribution } from "./TestHelpers/Data";
import { button } from "./TestHelpers/Page";
import * as API from "./TestHelpers/Api";

test.beforeEach(async ({ page }) => {
  await API.getWebsiteFeed(page);
});

test.describe("without being signed in", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "NOT_SIGNED_IN");
  });

  test("can view public project contribution (but can't merge)", async ({
    page,
  }) => {
    const projectRef = "@unison/base";
    const contrib = contribution(projectRef);
    await API.getProject(page, projectRef);
    await API.getProjectContribution(page, projectRef, contrib.number, contrib);
    await API.getProjectContributionTimeline(page, projectRef, contrib.number);
    await API.getProjectContributionMergeCheck_(
      page,
      projectRef,
      contrib.number,
      { status: 401 },
    );

    const response = await page.goto(
      `http://localhost:1234/${projectRef}/contributions/${contrib.number}`,
    );
    expect(response?.status()).toBeLessThan(400);

    const projectName = page.locator(".page-header").getByText(projectRef);
    await expect(projectName).toHaveClass("project-name");

    const contribNumber = page
      .locator(".page-title")
      .getByText(`#${contrib.number}`);
    await expect(contribNumber).toBeVisible();

    const contribTitle = page.locator(".page-title").getByText(contrib.title);
    await expect(contribTitle).toBeVisible();

    const contribDesc = page
      .locator(".contribution-description")
      .getByText(contrib.description);
    await expect(contribDesc).toBeVisible();

    await expect(button(page, "Browse Code")).toBeVisible();
    await expect(button(page, "View locally")).toBeVisible();
    await expect(button(page, "Merge Contribution")).not.toBeVisible();
  });

  test("can *not* view a private project contribution", async ({ page }) => {
    const projectRef = "@bob/private-project";
    const contrib = contribution(projectRef);
    await API.getProject_(page, projectRef, { status: 404 });
    await API.getProjectContribution_(page, projectRef, contrib.number, {
      data: contrib,
      status: 404,
    });
    await API.getProjectContributionTimeline_(
      page,
      projectRef,
      contrib.number,
      { status: 404 },
    );
    await API.getProjectContributionMergeCheck_(
      page,
      projectRef,
      contrib.number,
      { status: 401 },
    );

    const response = await page.goto(
      `http://localhost:1234/${projectRef}/contributions/${contrib.number}`,
    );
    expect(response?.status()).toBeLessThan(400);

    const projectName = page.locator(".page-header").getByText(projectRef);
    await expect(projectName).toHaveClass("project-name");

    const contribNumber = page
      .locator(".page-title")
      .getByText(`#${contrib.number}`);
    await expect(contribNumber).not.toBeVisible();

    const contribTitle = page.locator(".page-title").getByText(contrib.title);
    await expect(contribTitle).not.toBeVisible();

    const contribDesc = page
      .locator(".contribution-description")
      .getByText(contrib.description);
    await expect(contribDesc).not.toBeVisible();

    await expect(button(page, "Browse Code")).not.toBeVisible();
    await expect(button(page, "View locally")).not.toBeVisible();
    await expect(button(page, "Merge Contribution")).not.toBeVisible();

    await expect(page.getByText(`Couldn't find ${projectRef}`)).toBeVisible();
  });
});

test.describe("while signed in", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "@alice");
  });

  test.describe("with another user's private project and `project:view` permission", () => {
    test("can view a contribution (but can't merge)", async ({ page }) => {
      const projectRef = "@bob/private-project";
      const contrib = contribution(projectRef);
      await API.getProject(page, projectRef, {
        visibility: "private",
        permissions: ["project:view"],
      });
      await API.getProjectContribution(
        page,
        projectRef,
        contrib.number,
        contrib,
      );
      await API.getProjectContributionTimeline(
        page,
        projectRef,
        contrib.number,
      );
      await API.getProjectContributionMergeCheck_(
        page,
        projectRef,
        contrib.number,
        { status: 401 },
      );

      const response = await page.goto(
        `http://localhost:1234/${projectRef}/contributions/${contrib.number}`,
      );
      expect(response?.status()).toBeLessThan(400);

      const projectName = page.locator(".page-header").getByText(projectRef);
      await expect(projectName).toHaveClass("project-name");

      const contribNumber = page
        .locator(".page-title")
        .getByText(`#${contrib.number}`);
      await expect(contribNumber).toBeVisible();

      const contribTitle = page.locator(".page-title").getByText(contrib.title);
      await expect(contribTitle).toBeVisible();

      const contribDesc = page
        .locator(".contribution-description")
        .getByText(contrib.description);
      await expect(contribDesc).toBeVisible();

      await expect(button(page, "Browse Code")).toBeVisible();
      await expect(button(page, "View locally")).toBeVisible();
      await expect(button(page, "Merge Contribution")).not.toBeVisible();
    });
  });

  test.describe("with an another user's private project and `project:maintain` permission", () => {
    test("can view a contribution with a merge and archive button", async ({
      page,
    }) => {
      const projectRef = "@bob/private-project";
      const contrib = contribution(projectRef);
      await API.getProject(page, projectRef, {
        visibility: "private",
        permissions: ["project:maintain"],
      });
      await API.getProjectContribution(
        page,
        projectRef,
        contrib.number,
        contrib,
      );
      await API.getProjectContributionTimeline(
        page,
        projectRef,
        contrib.number,
      );
      await API.getProjectContributionMergeCheck(
        page,
        projectRef,
        contrib.number,
      );

      const response = await page.goto(
        `http://localhost:1234/${projectRef}/contributions/${contrib.number}`,
      );
      expect(response?.status()).toBeLessThan(400);

      const projectName = page.locator(".page-header").getByText(projectRef);
      await expect(projectName).toHaveClass("project-name");

      const contribNumber = page
        .locator(".page-title")
        .getByText(`#${contrib.number}`);
      await expect(contribNumber).toBeVisible();

      const contribTitle = page.locator(".page-title").getByText(contrib.title);
      await expect(contribTitle).toBeVisible();

      const contribDesc = page
        .locator(".contribution-description")
        .getByText(contrib.description);
      await expect(contribDesc).toBeVisible();

      await expect(button(page, "Browse Code")).toBeVisible();
      await expect(button(page, "View locally")).toBeVisible();
      await expect(button(page, "Merge Contribution")).toBeVisible();
      await expect(button(page, "Archive")).toBeVisible();
    });
  });
});
