// Backend API Stubs
import { Page } from "@playwright/test";
import { project, contributionTimeline, contribution, account } from "./Data";

async function getWebsiteFeed(page: Page) {
  const data = {
    version: "https://jsonfeed.org/version/1.1",
    title: "Unison Language | Blog",
    language: "en",
    home_page_url: "https://www.unison-lang.org/blog",
    feed_url: "https://www.unison-lang.org/feed.json",
    items: [],
  };

  return page.route(`*/**/website/feed.json`, async (request) => {
    await request.fulfill({ status: 200, json: data });
  });
}

// -- /account

async function getAccount(
  page: Page,
  handle: "NOT_SIGNED_IN" | string,
  accountData = {},
) {
  if (handle === "NOT_SIGNED_IN") {
    return get(page, {
      url: "account",
      status: 401,
    });
  } else {
    return get(page, {
      url: "account",
      status: 200,
      data: { ...account(handle), ...accountData },
    });
  }
}

// -- /users/:handle/project

async function getProject(page: Page, projectRef: string, projectData = {}) {
  return getProject_(page, projectRef, { status: 200, data: projectData });
}

async function getProject_(
  page: Page,
  projectRef: string,
  resp: { status: number; data?: {} },
) {
  const [handle, projectSlug] = projectRef.split("/");

  return get(page, {
    url: `/users/${handle.replace("@", "")}/projects/${projectSlug}`,
    status: resp.status,
    data: { ...project(projectRef), ...resp.data },
  });
}

async function getProjectReadme(page: Page, projectRef: string) {
  const [handle, projectSlug] = projectRef.split("/");

  return get(page, {
    url: `/users/${handle.replace("@", "")}/projects/${projectSlug}/readme`,
    status: 404,
    data: { readme: null },
  });
}

async function getProjectDependencies(
  page: Page,
  projectRef: string,
  release: string,
) {
  const [handle, projectSlug] = projectRef.split("/");

  return get(page, {
    url: `/users/${handle.replace(
      "@",
      "",
    )}/projects/${projectSlug}/releases/${release}/browse?namespace=lib`,
    status: 404,
  });
}

// -- /users/:handle/project/contributions/:contribution-ref
//
async function getProjectContribution(
  page: Page,
  projectRef: string,
  contribRef: number,
  contribData = {},
) {
  return getProjectContribution_(page, projectRef, contribRef, {
    status: 200,
    data: contribData,
  });
}

async function getProjectContribution_(
  page: Page,
  projectRef: string,
  contribRef: number,
  resp: { status: number; data?: {} },
) {
  const [handle, projectSlug] = projectRef.split("/");

  return get(page, {
    url: `/users/${handle.replace("@", "")}/projects/${projectSlug}/contributions/${contribRef}`,
    status: resp.status,
    data: { ...contribution(projectRef, contribRef), ...resp.data },
  });
}

async function getProjectContributionTimeline(
  page: Page,
  projectRef: string,
  contribRef: number,
) {
  return getProjectContributionTimeline_(page, projectRef, contribRef, {
    status: 200,
  });
}

async function getProjectContributionTimeline_(
  page: Page,
  projectRef: string,
  contribRef: number,
  resp: { status: number },
) {
  const [handle, projectSlug] = projectRef.split("/");

  return get(page, {
    url: `/users/${handle.replace("@", "")}/projects/${projectSlug}/contributions/${contribRef}/timeline`,
    status: resp.status,
    data: contributionTimeline(),
  });
}

async function getProjectContributionMergeCheck(
  page: Page,
  projectRef: string,
  contribRef: number,
) {
  return getProjectContributionMergeCheck_(page, projectRef, contribRef, {
    status: 200,
  });
}

async function getProjectContributionMergeCheck_(
  page: Page,
  projectRef: string,
  contribRef: number,
  resp: { status: number },
) {
  const [handle, projectSlug] = projectRef.split("/");

  return get(page, {
    url: `/users/${handle.replace("@", "")}/projects/${projectSlug}/contributions/${contribRef}/merge/check`,
    status: resp.status,
    data: { mergeability: { kind: "fast_forward" } },
  });
}

// -- UTIL

type Response = {
  url: string;
  data?: object;
  status: number;
};
async function get(page: Page, response: Response) {
  const url = response.url.startsWith("/") ? response.url : `/${response.url}`;

  return page.route(`*/**/api${url}`, async (request) => {
    if ("status" in response) {
      await request.fulfill({ status: response.status, json: response.data });
    } else {
      await request.fulfill({ json: response });
    }
  });
}

export {
  getWebsiteFeed,
  getAccount,
  getProject,
  getProject_,
  getProjectReadme,
  getProjectDependencies,
  getProjectContribution,
  getProjectContribution_,
  getProjectContributionTimeline,
  getProjectContributionTimeline_,
  getProjectContributionMergeCheck,
  getProjectContributionMergeCheck_,
  get,
};
