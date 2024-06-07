import SumType from "sums-up";

class Route extends SumType<{
  UserOverview: [string];
  ProjectOverview: [string, string];
  ProjectCode: [string, string, string | undefined];
  ProjectTickets: [string, string];
  ProjectContributions: [string, string];
  ProjectReleases: [string, string];
  ProjectBranches: [string, string];
  NotFound: [string];
}> {}

function UserOverview(handle: string): Route {
  return new Route("UserOverview", handle);
}

function ProjectOverview(handle: string, projectSlug: string): Route {
  return new Route("ProjectOverview", handle, projectSlug);
}

function ProjectCode(
  handle: string,
  projectSlug: string,
  branchRef?: string
): Route {
  return new Route("ProjectCode", handle, projectSlug, branchRef);
}

function ProjectTickets(handle: string, projectSlug: string): Route {
  return new Route("ProjectTickets", handle, projectSlug);
}

function ProjectContributions(handle: string, projectSlug: string): Route {
  return new Route("ProjectContributions", handle, projectSlug);
}

function ProjectReleases(handle: string, projectSlug: string): Route {
  return new Route("ProjectReleases", handle, projectSlug);
}

function ProjectBranches(handle: string, projectSlug: string): Route {
  return new Route("ProjectBranches", handle, projectSlug);
}

function NotFound(path: string): Route {
  return new Route("NotFound", path);
}

function parse(rawUrl: string): Route {
  const url = new URL(rawUrl);
  return fromPathname(url.pathname);
}

function fromPathname(rawPath: string): Route {
  const parts = rawPath.split("/").filter((s) => s.length);

  console.log("route parts", parts);

  const [handle, projectSlug, ...rest] = parts;

  if (handle && handle.startsWith("@") && !projectSlug) {
    return UserOverview(handle);
  } else if (handle && handle.startsWith("@") && projectSlug !== "p") {
    const [projectPage] = rest;

    if (projectPage === "code") {
      const [_, branchPart1, branchPart2] = rest;

      let branchRef = branchPart1;
      if (
        branchPart1 &&
        (branchPart1.startsWith("@") || branchPart1.startsWith("releases"))
      ) {
        branchRef = `${branchRef}/${branchPart2}`;
      }

      return ProjectCode(handle, projectSlug, branchRef);
    } else if (projectPage === "tickets") {
      return ProjectTickets(handle, projectSlug);
    } else if (projectPage === "contributions") {
      return ProjectContributions(handle, projectSlug);
    } else if (projectPage === "releases") {
      return ProjectReleases(handle, projectSlug);
    } else if (projectPage === "branches") {
      return ProjectBranches(handle, projectSlug);
    } else {
      return ProjectOverview(handle, projectSlug);
    }
  } else {
    return NotFound(rawPath);
  }
}

export {
  UserOverview,
  ProjectOverview,
  ProjectCode,
  ProjectContributions,
  ProjectTickets,
  ProjectReleases,
  NotFound,
  Route,
  parse,
  fromPathname,
};
