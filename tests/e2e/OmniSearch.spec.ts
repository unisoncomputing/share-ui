import { test, Page, expect } from "@playwright/test";
import * as API from "./TestHelpers/Api";

test.beforeEach(async ({ page }) => {
  await API.getAccount(page, "@testuser");
  await API.getCatalog(page);
  await API.getWebsiteFeed(page);
});

test("can see the search field", async ({ page }) => {
  const response = await page.goto("http://localhost:1234/");
  expect(response?.status()).toBeLessThan(400);

  await expect(searchField(page)).toBeVisible();
});

test.describe("definition search", () => {
  test.describe('query: "base" with definition matches and entity matches', () => {
    const query = "base";

    test.beforeEach(async ({ page }) => {
      await page.goto("http://localhost:1234/");
      await API.getDefinitionSearch(page, query);
      await API.getEntitySearch(page, `@${query}`);
      await API.getNameSearch(page, query);

      const field = searchField(page);
      await field.focus();
      await field.fill(query);
    });

    test("shows definitions matches", async ({ page }) => {
      await expect(searchResults(page)).toBeVisible();
      await expect(definitionMatches(page)).toHaveCount(3);
    });

    test("shows entity matches", async ({ page }) => {
      await expect(searchResults(page)).toBeVisible();
      await expect(projectMatches(page)).toHaveCount(2);
      await expect(userMatches(page)).toHaveCount(1);
    });
  });

  test.describe('query: "map" with only has definition matches', () => {
    const query = "map";

    test.beforeEach(async ({ page }) => {
      await page.goto("http://localhost:1234/");
      await API.getDefinitionSearch(page, query);
      await API.getEntitySearch(page, `@${query}`, []);
      await API.getNameSearch(page, query);

      const field = searchField(page);
      await field.focus();
      await field.fill(query);
    });

    test("shows definitions matches", async ({ page }) => {
      await expect(searchResults(page)).toBeVisible();
      await expect(definitionMatches(page)).toHaveCount(3);
    });

    test("doesn't show any entity matches", async ({ page }) => {
      await expect(searchResults(page)).toBeVisible();
      await expect(projectMatches(page)).toHaveCount(0);
      await expect(userMatches(page)).toHaveCount(0);
    });
  });

  test.describe('query: "base" with only entity matches', () => {
    const query = "base";

    test.beforeEach(async ({ page }) => {
      await page.goto("http://localhost:1234/");
      await API.getDefinitionSearch(page, query, []);
      await API.getEntitySearch(page, `@${query}`);
      await API.getNameSearch(page, query);
    });

    test("shows the entity matches", async ({ page }) => {
      const field = searchField(page);
      await field.focus();
      await field.fill(query);

      await expect(searchResults(page)).toBeVisible();
      await expect(projectMatches(page)).toHaveCount(2);
      await expect(userMatches(page)).toHaveCount(1);
    });

    test("doesn't show any definition matches", async ({ page }) => {
      const field = searchField(page);
      await field.focus();
      await field.fill(query);

      await expect(searchResults(page)).toBeVisible();
      await expect(definitionMatches(page)).toHaveCount(0);
    });
  });

  test.describe('query: "map" with no matches', () => {
    const query = "map";

    test.beforeEach(async ({ page }) => {
      await page.goto("http://localhost:1234/");
      await API.getDefinitionSearch(page, query, []);
      await API.getEntitySearch(page, `@${query}`, []);
      await API.getNameSearch(page, query);
    });

    test("doesn't show any matches", async ({ page }) => {
      const field = searchField(page);
      await field.focus();
      await field.fill(query);

      await expect(searchResults(page)).toBeVisible();
      await expect(projectMatches(page)).toHaveCount(0);
      await expect(userMatches(page)).toHaveCount(0);
      await expect(definitionMatches(page)).toHaveCount(0);
    });

    test("shows an empty state", async ({ page }) => {
      const field = searchField(page);
      await field.focus();
      await field.fill(query);

      await expect(searchResults(page)).toBeVisible();
      await expect(page.getByText("No matches found")).toBeVisible();
    });
  });

  test.describe("with more than 1 word", () => {
    const query = "[a] -> [a]";

    test.beforeEach(async ({ page }) => {
      await page.goto("http://localhost:1234/");
      await API.getDefinitionSearch(page, query);
      await API.getEntitySearch(page, `@${query}`);
      await API.getNameSearch(page, query);

      const field = searchField(page);
      await field.focus();
      await field.fill(query);
    });

    test("shows definitions matches", async ({ page }) => {
      await expect(searchResults(page)).toBeVisible();
      await expect(definitionMatches(page)).toHaveCount(3);
    });

    test("doesn't show any entity matches", async ({ page }) => {
      await expect(searchResults(page)).toBeVisible();
      await expect(projectMatches(page)).toHaveCount(0);
      await expect(userMatches(page)).toHaveCount(0);
    });
  });
});

test.describe("entity search", () => {
  test.describe('query "@unison" that has user and project matches, but no definition matches', () => {
    test.beforeEach(async ({ page }) => {
      await page.goto("http://localhost:1234/");
      await API.getDefinitionSearch(page, "unison", []);
      await API.getEntitySearch(page, "@unison");
      await API.getNameSearch(page, "unison");
    });

    test("shows entity matches", async ({ page }) => {
      const field = searchField(page);
      await field.focus();
      await field.fill("@unison");

      await expect(searchResults(page)).toBeVisible();
      await expect(projectMatches(page)).toHaveCount(2);
      await expect(userMatches(page)).toHaveCount(1);
    });

    test("doesn't show any definition matches", async ({ page }) => {
      const field = searchField(page);
      await field.focus();
      await field.fill("@unison");

      await expect(searchResults(page)).toBeVisible();
      await expect(definitionMatches(page)).toHaveCount(0);
    });
  });

  test.describe('query "@base" that has project matches and definition matches', () => {
    test.beforeEach(async ({ page }) => {
      await page.goto("http://localhost:1234/");
      await API.getDefinitionSearch(page, "base");
      await API.getEntitySearch(page, "@base");
      await API.getNameSearch(page, "base");

      const field = searchField(page);
      await field.focus();
      await field.fill("@base");
    });

    test("shows entity matches", async ({ page }) => {
      await expect(searchResults(page)).toBeVisible();
      await expect(projectMatches(page)).toHaveCount(2);
      await expect(userMatches(page)).toHaveCount(1);
    });

    test("shows definition matches", async ({ page }) => {
      await expect(searchResults(page)).toBeVisible();
      await expect(definitionMatches(page)).toHaveCount(3);
    });
  });

  test.describe('query "@base" with only definition matches', () => {
    const query = "@base";

    test.beforeEach(async ({ page }) => {
      await page.goto("http://localhost:1234/");
      await API.getDefinitionSearch(page, query.replace("@", ""));
      await API.getEntitySearch(page, query, []);
      await API.getNameSearch(page, query);
    });

    test("shows definition matches", async ({ page }) => {
      const field = searchField(page);
      await field.focus();
      await field.fill(query);

      await expect(searchResults(page)).toBeVisible();
      await expect(definitionMatches(page)).toHaveCount(3);
    });

    test("doesn't show any entity matches", async ({ page }) => {
      const field = searchField(page);
      await field.focus();
      await field.fill(query);

      await expect(searchResults(page)).toBeVisible();
      await expect(projectMatches(page)).toHaveCount(0);
      await expect(userMatches(page)).toHaveCount(0);
    });
  });

  test.describe('query "@nomatches" with no matches', () => {
    const query = "@nomatches";

    test.beforeEach(async ({ page }) => {
      await page.goto("http://localhost:1234/");
      await API.getDefinitionSearch(page, query.replace("@", ""), []);
      await API.getEntitySearch(page, query, []);
      await API.getNameSearch(page, query);
    });

    test("doesn't show any matches", async ({ page }) => {
      const field = searchField(page);
      await field.focus();
      await field.fill(query);

      await expect(searchResults(page)).toBeVisible();
      await expect(projectMatches(page)).toHaveCount(0);
      await expect(userMatches(page)).toHaveCount(0);
      await expect(definitionMatches(page)).toHaveCount(0);
    });

    test("shows an empty state", async ({ page }) => {
      const field = searchField(page);
      await field.focus();
      await field.fill(query);

      await expect(searchResults(page)).toBeVisible();
      await expect(page.getByText("No matches found")).toBeVisible();
    });
  });
});

// Helpers

function searchField(page: Page) {
  return page.locator("search").getByRole("textbox");
}

function searchResults(page: Page) {
  return page.locator(".main-result-sheet");
}

function definitionMatches(page: Page) {
  return page.locator(".definition-match");
}

function projectMatches(page: Page) {
  return page.locator(".project-match");
}

function userMatches(page: Page) {
  return page.locator(".user-match");
}
