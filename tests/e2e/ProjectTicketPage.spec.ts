import { test, expect } from "@playwright/test";
import { ticket } from "./TestHelpers/Data";
import { button } from "./TestHelpers/Page";
import * as API from "./TestHelpers/Api";

/*
test.beforeEach(async ({ page }) => {
  await API.getWebsiteFeed(page);
});

test.describe("while signed in", () => {
  test.beforeEach(async ({ page }) => {
    await API.getAccount(page, "@alice");
  });

  test("when closing, the project header number of open tickets is updated", async ({
    page,
  }) => {
    const projectRef = "@bob/private-project";
    const ticket_ = { ...ticket(projectRef), status: "open" };
    await API.getProject(page, projectRef, {
      visibility: "private",
      permissions: ["project:maintain"],
      numOpenTickets: 5,
    });
    await API.getProjectTicket(page, projectRef, ticket_.number, ticket_);
    await API.getProjectTicketTimeline(page, projectRef, ticket_.number);

    const response = await page.goto(
      `http://localhost:1234/${projectRef}/tickets/${ticket_.number}`,
    );
    expect(response?.status()).toBeLessThan(400);

    expect(
      page.locator(".page-header .min-md .tag.num-open-tickets").getByText("5"),
    ).toBeVisible();

    await API.patchProjectTicket(page, projectRef, ticket_.number, {
      status: 200,
      data: {
        ...ticket_,
        status: "closed",
      },
    });

    await button(page, "Close").click();

    expect(
      page.locator(".page-header .min-md .tag.num-open-tickets").getByText("4"),
    ).toBeVisible();
  });
});
*/
