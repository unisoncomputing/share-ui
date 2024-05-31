type UserOverview = { tag: "UserOverview"; handle: string };
type ProjectOverview = {
  tag: "ProjectOverview";
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

function NotFound(path: string): NotFound {
  return { tag: "NotFound", path };
}

type Route = UserOverview | ProjectOverview | NotFound;

function parse(rawUrl: string): Route {
  const url = new URL(rawUrl);
  return fromPathname(url.pathname);
}

function fromPathname(rawPath: string): Route {
  const parts = rawPath.split("/").filter((s) => s.length);

  const [handle, projectSlug, _rest] = parts;

  if (handle && handle.startsWith("@") && !projectSlug) {
    return UserOverview(handle);
  } else if (handle && handle.startsWith("@") && projectSlug !== "p") {
    return ProjectOverview(handle, projectSlug);
  } else {
    return NotFound(rawPath);
  }
}

type RoutePattern<T> = {
  UserOverview(handle: string): T;
  ProjectOverview(handle: string, projectSlug: string): T;
  NotFound(url: string): T;
};

function match<T>(route: Route, pattern: RoutePattern<T>): T {
  switch (route.tag) {
    case "UserOverview":
      return pattern.UserOverview(route.handle);
    case "ProjectOverview":
      return pattern.ProjectOverview(route.handle, route.projectSlug);
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
