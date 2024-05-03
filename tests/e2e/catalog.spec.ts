import { test, expect } from "@playwright/test";

const TIMEOUT = 500;

test.describe("Catalog", () => {
  test("Simulate the Checkly check", async ({ page }) => {
    await page.route("*/**/api/account", async (route) => {
      const json = {
        avatarUrl: null,
        completedTours: ["welcome-terms"],
        handle: "testuser",
        name: "Test User",
        organizationMemberships: [],
        primaryEmail: "test@example.com",
        userId: "testuserid",
      };

      await route.fulfill({ json });
    });

    await page.route("*/**/api/catalog", async (route) => {
      const json = [
        {
          name: "Featured",
          projects: [
            {
              createdAt: "2023-05-25T01:39:01.955533Z",
              isFaved: true,
              numFavs: 4,
              owner: {
                handle: "@unison",
                name: "Unison",
                type: "organization",
              },
              slug: "base",
              summary: "The unison base library.",
              tags: [],
              updatedAt: "2023-06-05T02:37:25.346367Z",
              visibility: "public",
            },
            {
              createdAt: "2023-04-03T17:05:08.873717Z",
              isFaved: false,
              numFavs: 5,
              owner: {
                handle: "@unison",
                name: "Unison",
                type: "organization",
              },
              slug: "distributed",
              summary:
                "A library for distributed computing. Computations can be run locally or on unison.cloud.",
              tags: [],
              updatedAt: "2023-04-05T02:38:23.774294Z",
              visibility: "public",
            },
          ],
        },
      ];

      await route.fulfill({ json });
    });

    const response = await page.goto("http://localhost:1234/");
    // Test that the response did not fail
    expect(response?.status()).toBeLessThan(400);

    const base = page.getByTitle("@unison/base");
    await expect(base).toHaveClass("project-name", { timeout: TIMEOUT });
    const distributed = page.getByTitle("@unison/distributed");
    await expect(distributed).toHaveClass("project-name", { timeout: TIMEOUT });
  });
});
