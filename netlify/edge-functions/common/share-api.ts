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

const ShareAPI = {
  baseURL: "https://api.unison-lang.org",

  getUser: async (handle: string): Promise<APIUser> => {
    return fetch(`${ShareAPI.baseURL}/users/${handle.replace("@", "")}`).then(
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
    return fetch(
      `${ShareAPI.baseURL}/users/${handle.replace(
        "@",
        ""
      )}/projects/${projectSlug}`
    ).then((resp) => {
      console.log(resp);

      if (!resp.ok) {
        throw new Error(resp.statusText);
      }

      return resp.json() as Promise<APIProject>;
    });
  },

  getContribution: async (
    _handle: string,
    _projectSlug: string,
    _contribRef: string
  ) => {},

  getDefinition: async (
    _handle: string,
    _projectSlug: string,
    _fqn: Array<string>
  ) => {},
};

export default ShareAPI;
