import SumType from "sums-up";

class Route extends SumType<{
  UserOverview: [string];
  ProjectOverview: [string, string];
  ProjectCode: [string, string, string | undefined];
  ProjectDefinition: [string, string, string, string, Array<string>];
  ProjectTickets: [string, string];
  ProjectTicket: [string, string, number];
  ProjectContributions: [string, string];
  ProjectContribution: [string, string, number];
  ProjectReleases: [string, string];
  ProjectRelease: [string, string, string];
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

function ProjectDefinition(
  handle: string,
  projectSlug: string,
  branchRef: string,
  definitionType: string,
  fqn: Array<string>
): Route {
  return new Route(
    "ProjectDefinition",
    handle,
    projectSlug,
    branchRef,
    definitionType,
    fqn
  );
}

function ProjectTickets(handle: string, projectSlug: string): Route {
  return new Route("ProjectTickets", handle, projectSlug);
}

function ProjectTicket(
  handle: string,
  projectSlug: string,
  ticketRef: number
): Route {
  return new Route("ProjectTicket", handle, projectSlug, ticketRef);
}

function ProjectContributions(handle: string, projectSlug: string): Route {
  return new Route("ProjectContributions", handle, projectSlug);
}

function ProjectContribution(
  handle: string,
  projectSlug: string,
  contributionRef: number
): Route {
  return new Route("ProjectContribution", handle, projectSlug, contributionRef);
}

function ProjectReleases(handle: string, projectSlug: string): Route {
  return new Route("ProjectReleases", handle, projectSlug);
}

function ProjectRelease(
  handle: string,
  projectSlug: string,
  version: string
): Route {
  return new Route("ProjectRelease", handle, projectSlug, version);
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

function isContributorBranchOrRelease(s: string): boolean {
  return s.startsWith("@") || s.startsWith("releases");
}

function fromPathname(rawPath: string): Route {
  const parts = rawPath.split("/").filter((s) => s.length);

  const [handle, projectSlug, ...rest] = parts;

  if (handle && handle.startsWith("@") && !projectSlug) {
    return UserOverview(handle);
  } else if (handle && handle.startsWith("@") && projectSlug !== "p") {
    const [projectPage] = rest;

    if (projectPage === "code") {
      const [
        _,
        branchPart1,
        branchPart2,
        namespaceHashOrDefinitionType,
        definitionTypeOrNameSegment,
        ...nameSegments
      ] = rest;

      let branchRef = branchPart1;
      let fqn = nameSegments || [];
      let definitionType = definitionTypeOrNameSegment;
      if (branchPart1 && isContributorBranchOrRelease(branchPart1)) {
        branchRef = `${branchRef}/${branchPart2}`;
      } else if (branchPart2) {
        // if the branchPart1 contributor branch a release, then branch2 is a namespaceHash,
        // which means the namespaceHashOrDefinitionType is a definition type, and definitionTypeOrNameSegment
        // is the first part of the FQN
        fqn = [definitionTypeOrNameSegment, ...nameSegments];
        definitionType = namespaceHashOrDefinitionType;
      }

      // Gotta have an FQN to be a definition Route
      if (fqn.length && definitionType) {
        return ProjectDefinition(
          handle,
          projectSlug,
          branchRef,
          // remove the trailing 's' from the definition type
          definitionType.substring(0, definitionType.length - 1),
          fqn
        );
      } else {
        return ProjectCode(handle, projectSlug, branchRef);
      }
    } else if (projectPage === "tickets") {
      const [_, ticketRef] = rest;

      if (!isNaN(parseInt(ticketRef))) {
        return ProjectTicket(handle, projectSlug, parseInt(ticketRef));
      } else {
        return ProjectTickets(handle, projectSlug);
      }
    } else if (projectPage === "contributions") {
      const [_, contribRef] = rest;

      if (!isNaN(parseInt(contribRef))) {
        return ProjectContribution(handle, projectSlug, parseInt(contribRef));
      } else {
        return ProjectContributions(handle, projectSlug);
      }
    } else if (projectPage === "releases") {
      const [_, version] = rest;

      if (version) {
        return ProjectRelease(handle, projectSlug, version);
      } else {
        return ProjectReleases(handle, projectSlug);
      }
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
  ProjectDefinition,
  ProjectContributions,
  ProjectContribution,
  ProjectTickets,
  ProjectTicket,
  ProjectReleases,
  NotFound,
  Route,
  parse,
  fromPathname,
};
