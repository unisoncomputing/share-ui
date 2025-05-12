import { faker } from "@faker-js/faker";
import { test, expect } from "@playwright/test";

import { contribution } from "./TestHelpers/Data";
import * as API from "./TestHelpers/Api";

const projectRef = "@unison/base";
const contrib = contribution(projectRef);

test.beforeEach(async ({ page }) => {
  await API.getWebsiteFeed(page);
  await API.getAccount(page, "@alice");
  await API.getProject(page, projectRef);
  await API.getProjectContribution(page, projectRef, contrib.number, contrib);
});

test.describe("When the branch diff is still computing", () => {
  test.beforeEach(async ({ page }) => {
    await API.getProjectContributionDiff(page, projectRef, contrib.number, {
      tag: "computing",
    });
    await page.goto(
      `http://localhost:1234/${projectRef}/contributions/${contrib.number}/changes`,
    );
  });

  test('it shows a "Computing..." message', async ({ page }) => {
    await expect(page.getByText("still being computed...")).toBeVisible();
  });
});

test.describe("When the branch diff is computed", () => {
  test.beforeEach(async ({ page }) => {
    await API.getProjectContributionDiff(page, projectRef, contrib.number, {
      tag: "ok",
    });
    await page.goto(
      `http://localhost:1234/${projectRef}/contributions/${contrib.number}/changes`,
    );
  });

  test("it shows a the changes", async ({ page }) => {
    await expect(page.locator(".definition-changes .change-line")).toHaveCount(
      3,
    );
  });
});

test.describe("When the branch diff couldn't be computed", () => {
  test.describe('error: "ImpossibleError"', () => {
    test.beforeEach(async ({ page }) => {
      await API.getProjectContributionDiff(page, projectRef, contrib.number, {
        tag: "error",
        error: { tag: "impossibleError", oldOrNewBranch: "new" },
      });
      await page.goto(
        `http://localhost:1234/${projectRef}/contributions/${contrib.number}/changes`,
      );
    });

    test("it shows an error message", async ({ page }) => {
      await expect(
        page.getByText(
          "Unfortunately the contribution diff could not be computed",
        ),
      ).toBeVisible();
    });
  });

  test.describe('error: "ConstructorAlias"', () => {
    const typeName = faker.lorem.word();
    const constructorName1 = faker.lorem.word();
    const constructorName2 = faker.lorem.word();
    test.beforeEach(async ({ page }) => {
      await API.getProjectContributionDiff(page, projectRef, contrib.number, {
        tag: "error",
        error: {
          tag: "constructorAlias",
          oldOrNewBranch: "new",
          typeName,
          constructorName1,
          constructorName2,
        },
      });
      await page.goto(
        `http://localhost:1234/${projectRef}/contributions/${contrib.number}/changes`,
      );
    });

    test("it shows an error message", async ({ page }) => {
      await expect(
        page.getByText(
          "Unfortunately the contribution diff could not be computed",
        ),
      ).toBeVisible();
      await expect(
        page.getByText(
          `The type ${typeName} has a constructor alias: ${constructorName1} and ${constructorName2} on the ${contrib.sourceBranchRef} branch.`,
        ),
      ).toBeVisible();
    });
  });

  test.describe('error: "MissingConstructorName"', () => {
    const typeName = faker.lorem.word();
    test.beforeEach(async ({ page }) => {
      await API.getProjectContributionDiff(page, projectRef, contrib.number, {
        tag: "error",
        error: {
          tag: "missingConstructorName",
          oldOrNewBranch: "new",
          typeName,
        },
      });
      await page.goto(
        `http://localhost:1234/${projectRef}/contributions/${contrib.number}/changes`,
      );
    });

    test("it shows an error message", async ({ page }) => {
      await expect(
        page.getByText(
          "Unfortunately the contribution diff could not be computed",
        ),
      ).toBeVisible();
      await expect(
        page.getByText(
          `The type ${typeName} is missing a constructor name on the ${contrib.sourceBranchRef} branch.`,
        ),
      ).toBeVisible();
    });
  });

  test.describe('error: "NestedDeclAlias"', () => {
    const constructorName1 = faker.lorem.word();
    const constructorName2 = faker.lorem.word();
    test.beforeEach(async ({ page }) => {
      await API.getProjectContributionDiff(page, projectRef, contrib.number, {
        tag: "error",
        error: {
          tag: "nestedDeclAlias",
          oldOrNewBranch: "new",
          constructorName1,
          constructorName2,
        },
      });
      await page.goto(
        `http://localhost:1234/${projectRef}/contributions/${contrib.number}/changes`,
      );
    });

    test("it shows an error message", async ({ page }) => {
      await expect(
        page.getByText(
          "Unfortunately the contribution diff could not be computed",
        ),
      ).toBeVisible();
      await expect(
        page.getByText(
          `On the ${contrib.sourceBranchRef} branch, the type ${constructorName1} is an alias of ${constructorName2}.`,
        ),
      ).toBeVisible();
      await expect(
        page.getByText(
          "It's nested under an alias of itself. Please separate them or delete one copy.",
        ),
      ).toBeVisible();
    });
  });

  test.describe('error: "StrayConstructor"', () => {
    const constructorName = faker.lorem.word();
    test.beforeEach(async ({ page }) => {
      await API.getProjectContributionDiff(page, projectRef, contrib.number, {
        tag: "error",
        error: {
          tag: "strayConstructor",
          oldOrNewBranch: "new",
          constructorName,
        },
      });
      await page.goto(
        `http://localhost:1234/${projectRef}/contributions/${contrib.number}/changes`,
      );
    });

    test("it shows an error message", async ({ page }) => {
      await expect(
        page.getByText(
          "Unfortunately the contribution diff could not be computed",
        ),
      ).toBeVisible();
      await expect(
        page.getByText(
          `The constructor ${constructorName} is orphaned on the ${contrib.sourceBranchRef} branch.`,
        ),
      ).toBeVisible();
    });
  });
});
