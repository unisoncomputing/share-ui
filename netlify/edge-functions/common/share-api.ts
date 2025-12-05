import { APIDefinitions } from "./definition.ts";
import * as Http from "./http";

type APIProject = {
  owner: { handle: string };
  slug: string;
  summary: string;
  tags: Array<string>;
  numActiveContributions: number;
  numOpenTickets: number;
  numFavs: number;
  latestRelease?: string;
  defaultBranch?: string;
};

type APIUser = {
  handle: string;
  name?: string;
  avatarUrl?: string;
  bio?: string;
  kind: "user";
};

type APIOrg = {
  user: APIUser;
  kind: "org";
};

type APIProfile = APIUser | APIOrg;

type Author = {
  handle: string;
  name?: string;
  avatarUrl?: string;
};

type APITicket = {
  author?: Author | string;
  title: string;
  description: string;
  numComments: 0;
  status: "open" | "closed";
  createdAt: string;
  updatedAt: string;
};

type APIContribution = {
  author?: Author | string;
  title: string;
  description?: string;
  numComments: number;
  sourceBranchRef: string;
  status: "draft" | "in_review" | "merged" | "closed";
  targetBranchRef: string;
  createdAt: string;
  updatedAt: string;
};

type APIRelease = {
  causalHashSquashed: string;
  causalHashUnsquashed: string;
  createdAt: string;
  createdBy?: Author | string;
  updatedAt: string;
  version: string;
};

async function error(url: string, resp: Response): Promise<Error> {
  const body = await resp.text();
  return Error(`GET ${url}: ${resp.statusText} | ${body}`);
}

function apiHandle(handle: string): string {
  return handle.replace("@", "");
}

const ShareAPI = {
  baseURL: "https://api.unison-lang.org",

  projectBaseUrl: (handle: string, projectSlug: string, path?: string) => {
    return `${ShareAPI.baseURL}/users/${apiHandle(
      handle,
    )}/projects/${projectSlug}${path || ""}`;
  },

  getProfile: async (handle: string): Promise<APIProfile> => {
    const url = `${ShareAPI.baseURL}/users/${apiHandle(handle)}`;

    return Http.get(url).then(async (resp) => {
      if (!resp.ok) {
        throw await error(url, resp);
      }

      return resp.json() as Promise<APIProfile>;
    });
  },

  getProject: async (
    handle: string,
    projectSlug: string,
  ): Promise<APIProject> => {
    const url = ShareAPI.projectBaseUrl(handle, projectSlug);

    return Http.get(url).then(async (resp) => {
      if (!resp.ok) {
        throw await error(url, resp);
      }

      return resp.json() as Promise<APIProject>;
    });
  },

  getContribution: async (
    handle: string,
    projectSlug: string,
    contribRef: number,
  ): Promise<APIContribution> => {
    const url = ShareAPI.projectBaseUrl(
      handle,
      projectSlug,
      `/contributions/${contribRef}`,
    );
    return Http.get(url).then(async (resp) => {
      if (!resp.ok) {
        throw await error(url, resp);
      }

      return resp.json() as Promise<APIContribution>;
    });
  },

  getTicket: async (
    handle: string,
    projectSlug: string,
    ticketRef: number,
  ): Promise<APITicket> => {
    const url = ShareAPI.projectBaseUrl(
      handle,
      projectSlug,
      `/tickets/${ticketRef}`,
    );
    return Http.get(url).then(async (resp) => {
      if (!resp.ok) {
        throw await error(url, resp);
      }

      return resp.json() as Promise<APITicket>;
    });
  },

  getRelease: async (
    handle: string,
    projectSlug: string,
    version: string,
  ): Promise<APIRelease> => {
    const url = ShareAPI.projectBaseUrl(
      handle,
      projectSlug,
      `/releases/${version}`,
    );

    return Http.get(url).then(async (resp) => {
      if (!resp.ok) {
        throw await error(url, resp);
      }

      return resp.json() as Promise<APIRelease>;
    });
  },

  getDefinition: async (
    handle: string,
    projectSlug: string,
    branchRef: string,
    fqn: Array<string>,
  ): Promise<APIDefinitions> => {
    function mkUrl(branchPart: string): string {
      return ShareAPI.projectBaseUrl(
        handle,
        projectSlug,
        `/${branchPart}/definitions/by-name/${fqn.join(".")}`,
      );
    }

    let url = mkUrl(`branches/${branchRef.replace("/", "%2F")}`);
    if (branchRef.startsWith("releases")) {
      url = mkUrl(branchRef);
    }

    return Http.get(url).then(async (resp) => {
      if (!resp.ok) {
        throw await error(url, resp);
      }

      return resp.json() as Promise<APIDefinitions>;
    });
  },
};

export default ShareAPI;
export { ShareAPI };
