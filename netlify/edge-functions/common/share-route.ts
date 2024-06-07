type UserOverview = { tag: "UserOverview"; handle: string };
type ProjectOverview = {
  tag: "ProjectOverview";
  handle: string;
  projectSlug: string;
};
type ProjectCode = {
  tag: "ProjectCode";
  handle: string;
  projectSlug: string;
  branchRef?: string;
};
type ProjectTickets = {
  tag: "ProjectTickets";
  handle: string;
  projectSlug: string;
};
type ProjectContributions = {
  tag: "ProjectContributions";
  handle: string;
  projectSlug: string;
};
type NotFound = { tag: "NotFound"; path: string };

function UserOverview(handle: string): UserOverview {
  return { tag: "UserOverview", handle };
}

function ProjectOverview(handle: string, projectSlug: string): ProjectOverview {
  return { tag: "ProjectOverview", handle, projectSlug };
}

function ProjectCode(
  handle: string,
  projectSlug: string,
  branchRef?: string
): ProjectCode {
  return { tag: "ProjectCode", handle, projectSlug, branchRef };
}

function ProjectTickets(handle: string, projectSlug: string): ProjectTickets {
  return { tag: "ProjectTickets", handle, projectSlug };
}

function ProjectContributions(
  handle: string,
  projectSlug: string
): ProjectContributions {
  return { tag: "ProjectContributions", handle, projectSlug };
}

function NotFound(path: string): NotFound {
  return { tag: "NotFound", path };
}

type Route =
  | UserOverview
  | ProjectOverview
  | ProjectCode
  | ProjectTickets
  | ProjectContributions
  | NotFound;

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
    } else {
      return ProjectOverview(handle, projectSlug);
    }
  } else {
    return NotFound(rawPath);
  }
}

type RoutePattern<T> = {
  UserOverview(handle: string): T;
  ProjectOverview(handle: string, projectSlug: string): T;
  ProjectCode(handle: string, projectSlug: string, branchRef?: string): T;
  ProjectTickets(handle: string, projectSlug: string): T;
  ProjectContributions(handle: string, projectSlug: string): T;
  NotFound(url: string): T;
};

function match<T>(route: Route, pattern: RoutePattern<T>): T {
  switch (route.tag) {
    case "UserOverview":
      return pattern.UserOverview(route.handle);
    case "ProjectOverview":
      return pattern.ProjectOverview(route.handle, route.projectSlug);
    case "ProjectCode":
      return pattern.ProjectCode(
        route.handle,
        route.projectSlug,
        route.branchRef
      );
    case "ProjectTickets":
      return pattern.ProjectTickets(route.handle, route.projectSlug);
    case "ProjectContributions":
      return pattern.ProjectContributions(route.handle, route.projectSlug);
    case "NotFound":
      return pattern.NotFound(route.path);
  }
}

export {
  UserOverview,
  ProjectOverview,
  NotFound,
  Route,
  parse,
  match,
  fromPathname,
};
