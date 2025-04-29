import { test, expect } from "@playwright/test";
import * as API from "./TestHelpers/Api";

test("Simulate the Checkly check", async ({ page }) => {
  await API.getAccount(page, "@testuser");
  await API.getCatalog(page);

  const response = await page.goto("http://localhost:1234/");
  expect(response?.status()).toBeLessThan(400);

  const base = page.getByTitle("@unison/base");
  await expect(base).toHaveClass("project-name");
  const distributed = page.getByTitle("@unison/distributed");
  await expect(distributed).toHaveClass("project-name");
});
