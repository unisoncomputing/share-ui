import { APIDefinition } from "./definition.ts";

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
};

type APITicket = {
  author?: string;
  title: string;
  description: string;
  numComments: 0;
  status: "open" | "closed";
  createdAt: string;
  updatedAt: string;
};

type APIContribution = {
  author?: string;
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
  createdBy?: string;
  updatedAt: string;
  version: string;
};

function apiHandle(handle: string): string {
  return handle.replace("@", "");
}

const ShareAPI = {
  baseURL: "https://api.unison-lang.org",

  projectBaseUrl: (handle: string, projectSlug: string, path?: string) => {
    return `${ShareAPI.baseURL}/users/${apiHandle(
      handle
    )}/projects/${projectSlug}${path || ""}`;
  },

  getUser: async (handle: string): Promise<APIUser> => {
    return fetch(`${ShareAPI.baseURL}/users/${apiHandle(handle)}`).then(
      (resp) => {
        if (!resp.ok) {
          throw new Error(resp.statusText);
        }

        return resp.json() as Promise<APIUser>;
      }
    );
  },

  getProject: async (
    handle: string,
    projectSlug: string
  ): Promise<APIProject> => {
    return fetch(ShareAPI.projectBaseUrl(handle, projectSlug)).then((resp) => {
      if (!resp.ok) {
        throw new Error(resp.statusText);
      }

      return resp.json() as Promise<APIProject>;
    });
  },

  getContribution: async (
    handle: string,
    projectSlug: string,
    contribRef: number
  ): Promise<APIContribution> => {
    return fetch(
      ShareAPI.projectBaseUrl(
        handle,
        projectSlug,
        `/contributions/${contribRef}`
      )
    ).then((resp) => {
      if (!resp.ok) {
        throw new Error(resp.statusText);
      }

      return resp.json() as Promise<APIContribution>;
    });
  },

  getTicket: async (
    handle: string,
    projectSlug: string,
    ticketRef: number
  ): Promise<APITicket> => {
    return fetch(
      ShareAPI.projectBaseUrl(handle, projectSlug, `/tickets/${ticketRef}`)
    ).then((resp) => {
      if (!resp.ok) {
        throw new Error(resp.statusText);
      }

      return resp.json() as Promise<APITicket>;
    });
  },

  getRelease: async (
    handle: string,
    projectSlug: string,
    version: string
  ): Promise<APIRelease> => {
    return fetch(
      ShareAPI.projectBaseUrl(handle, projectSlug, `/releases/${version}`)
    ).then((resp) => {
      if (!resp.ok) {
        throw new Error(resp.statusText);
      }

      return resp.json() as Promise<APIRelease>;
    });
  },

  getDefinition: async (
    handle: string,
    projectSlug: string,
    branchRef: string,
    fqn: Array<string>
  ): Promise<APIDefinitions> => {
    function mkUrl(branchPart: string): string {
      return ShareAPI.projectBaseUrl(
        handle,
        projectSlug,
        `/${branchPart}/definitions/by-name/${fqn.join(".")}`
      );
    }

    let url = mkUrl(`branches/${branchRef.replace("/", "%2F")}`);
    if (branchRef.startsWith("releases")) {
      url = mkUrl(branchRef);
    }

    return fetch(url).then((resp) => {
      if (!resp.ok) {
        throw new Error(resp.statusText);
      }

      return resp.json() as Promise<APIDefinitions>;
    });
  },
};

export default ShareAPI;
export { ShareAPI };
