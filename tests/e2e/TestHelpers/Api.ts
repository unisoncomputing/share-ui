// Backend API Stubs
import { Page } from "@playwright/test";
import { project, account } from "./Fixtures";

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
  getAccount,
  getProject,
  getProject_,
  getProjectReadme,
  getProjectDependencies,
  get,
};
