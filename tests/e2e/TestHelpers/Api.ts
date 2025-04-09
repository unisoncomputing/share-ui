// Backend API Stubs
import { Page } from "@playwright/test";
import {
  project,
  contributionTimeline,
  userDetails,
  org,
  contribution,
  user,
  account,
} from "./Data";

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

// -- /catalog

async function getCatalog(page: Page) {
  const data = [
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

  return get(page, { url: "catalog", status: 200, data });
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

async function getUserProfile(page: Page, handle: string, userData = {}) {
  return get(page, {
    url: `users/${handle.replace("@", "")}`,
    status: 200,
    data: { ...userDetails(handle), ...userData },
  });
}

// -- ORGS

async function getOrgProfile(page: Page, handle: string, orgData = {}) {
  return get(page, {
    url: `users/${handle.replace("@", "")}`,
    status: 200,
    data: { ...org(handle), ...orgData },
  });
}

async function getOrgRoleAssignments(
  page: Page,
  handle: string,
  assignments = null,
) {
  return getOrgRoleAssignments_(page, handle, {
    status: 200,
    data: assignments,
  });
}

async function getOrgRoleAssignments_(
  page: Page,
  handle: string,
  resp: { status: number; data?: unknown[] },
) {
  function roleAssignment(roles: string[]) {
    return {
      roles: roles,
      subject: { data: user(), kind: "user" },
    };
  }

  return get(page, {
    url: `/orgs/${handle.replace("@", "")}/roles`,
    status: resp.status,
    data: {
      role_assignments: resp.data || [
        roleAssignment(["org_admin"]),
        roleAssignment(["org_viewer"]),
        roleAssignment(["org_owner"]),
      ],
    },
  });
}

// -- /users/:handle/projects
//
async function getProjects(page: Page, handle: string) {
  return get(page, {
    url: `/users/${handle.replace("@", "")}/projects`,
    status: 200,
    data: [project(), project(), project()],
  });
}

// -- /users/:handle/project/:slug

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
  getCatalog,
  getAccount,
  getUserProfile,
  getOrgProfile,
  getOrgRoleAssignments,
  getOrgRoleAssignments_,
  getProjects,
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
