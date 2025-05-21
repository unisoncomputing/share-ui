import { faker } from "@faker-js/faker";

function account(handle: string) {
  return {
    ...user(),
    handle: handle.replace("@", ""),
    completedTours: ["welcome-terms"],
    isSuperadmin: false,
    organizationMemberships: [],
    primaryEmail: faker.internet.email(),
  };
}

function userHandle() {
  return faker.lorem.slug(1);
}

function user(handle?: string) {
  const firstName = faker.person.firstName();
  const lastName = faker.person.lastName();
  const handle_ = handle ? handle : userHandle();

  return {
    avatarUrl: faker.image.avatar(),
    handle: handle_.replace("@", ""),
    name: `${firstName} ${lastName}`,
    userId: faker.string.uuid(),
    kind: "user",
  };
}

function userDetails(handle?: string) {
  const firstName = faker.person.firstName();
  const lastName = faker.person.lastName();
  const handle_ = handle ? handle : faker.lorem.slug(1);

  return {
    avatarUrl: faker.image.avatar(),
    handle: handle_.replace("@", ""),
    name: `${firstName} ${lastName}`,
    userId: faker.string.uuid(),
    website: faker.internet.url(),
    location: null,
    bio: faker.person.bio(),
    pronouns: null,
    kind: "user",
  };
}

function org(handle?: string) {
  const handle_ = handle ? handle : faker.lorem.slug(1);

  return {
    user: {
      avatarUrl: faker.image.avatar(),
      handle: handle_.replace("@", ""),
      name: faker.company.name(),
      userId: faker.string.uuid(),
    },
    orgId: faker.string.uuid(),
    kind: "org",
    permissions: [],
    isCommercial: false,
  };
}

function projectRef(handle?: string) {
  const handle_ = handle ? handle : faker.lorem.slug(1);
  const slug = faker.lorem.slug(2);
  return `@${handle_}/${slug}`;
}

function branchRef(handle?: string) {
  const handle_ = handle ? handle : faker.lorem.slug(1);
  const slug = faker.lorem.slug(2);
  return `@${handle_}/${slug}`;
}

function branchSlugOf(branchRef: string) {
  const parts = branchRef.split("/");

  return parts[parts.length - 1];
}

function hash() {
  return `#${faker.string.alphanumeric(256)}`;
}

function project(ref?: string) {
  const ref_ = ref ? ref : projectRef();
  const [handle, slug] = ref_.split("/");

  return {
    createdAt: faker.date.past(),
    defaultBranch: "main",
    isFaved: true,
    latestRelease: "3.33.0",
    numActiveContributions: faker.number.int(5),
    numClosedContributions: faker.number.int(100),
    numClosedTickets: faker.number.int(100),
    numDraftContributions: faker.number.int(2),
    numFavs: faker.number.int(1000),
    numMergedContributions: faker.number.int(10),
    numOpenTickets: faker.number.int(5),
    owner: {
      handle: handle,
      name: handle.replace("@", ""),
      type: "organization",
    },
    permissions: [],
    releaseDownloads: [
      faker.number.int(100),
      faker.number.int(100),
      faker.number.int(100),
      faker.number.int(100),
      faker.number.int(100),
    ],
    slug: slug,
    summary: faker.lorem.text(),
    tags: [],
    updatedAt: faker.date.past(),
    visibility: "public",
  };
}

type ContributionStatus = "draft" | "in_review" | "merged" | "closed";

function contributionStatus(): ContributionStatus {
  return faker.helpers.arrayElement(["draft", "in_review", "merged", "closed"]);
}

function contribution(projectRef: string, contribRef?: number) {
  const author = user();

  return {
    author: author,
    contributionStateToken: faker.string.uuid(),
    createdAt: faker.date.past(),
    description: faker.lorem.paragraphs(),
    id: faker.string.uuid(),
    numComments: faker.number.int(5),
    number: contribRef || faker.number.int(100),
    projectRef: projectRef,
    sourceBranchRef: `@${author.handle}/${faker.lorem.slug()}`,
    status: contributionStatus(),
    targetBranchRef: "main",
    title: faker.lorem.sentences(1),
    updatedAt: faker.date.past(),
  };
}

function contributionStatusChangeEvent(
  newStatus = "in_review",
  oldStatus: string | null = null,
) {
  return {
    actor: user(),
    kind: "statusChange",
    newStatus: newStatus,
    oldStatus: oldStatus,
    timestamp: faker.date.past(),
  };
}

function timelineCommentEvent() {
  return {
    actor: user(),
    content: faker.lorem.paragraphs(2),
    editedAt: null,
    id: `CMT-${faker.string.uuid()}`,
    kind: "comment",
    revision: 0,
    timestamp: faker.date.past(),
  };
}

function contributionTimeline(events?: unknown[]) {
  return {
    cursor: faker.string.uuid(),
    items: events || [
      contributionStatusChangeEvent(),
      timelineCommentEvent(),
      timelineCommentEvent(),
    ],
  };
}

function changeLine() {
  return {
    contents: {
      contents: {
        fullName: "data.Map.fromListWithKey",
        newHash: hash(),
        newTag: "Plain",
        oldHash: hash(),
        oldTag: "Plain",
        shortName: "fromListWithKey",
      },
      tag: "Updated",
    },
    tag: "Plain",
  };
}

type DiffErrorCulprit = "new" | "old";

type DiffErrorDetails =
  | { tag: "impossibleError" }
  | {
      tag: "constructorAlias";
      oldOrNewBranch: DiffErrorCulprit;
      typeName: string;
      constructorName1: string;
      constructorName2: string;
    }
  | {
      tag: "missingConstructorName";
      oldOrNewBranch: DiffErrorCulprit;
      typeName: string;
    }
  | {
      tag: "nestedDeclAlias";
      oldOrNewBranch: DiffErrorCulprit;
      constructorName1: string;
      constructorName2: string;
    }
  | {
      tag: "strayConstructor";
      oldOrNewBranch: DiffErrorCulprit;
      constructorName: string;
    };

type ContributionDiffConfig =
  | { tag: "ok" }
  | { tag: "computing" }
  | { tag: "error"; error: DiffErrorDetails };

function contributionDiff(projectRef: string, cfg: ContributionDiffConfig) {
  switch (cfg.tag) {
    case "ok":
      return {
        tag: "done",
        diff: {
          tag: "ok",
          defns: {
            changes: [changeLine(), changeLine(), changeLine()],
            children: [],
          },
          libdeps: [],
        },
        newRef: branchRef(),
        newRefHash: hash(),
        oldRef: "main",
        oldRefHash: hash(),
        project: projectRef,
      };
    case "computing":
      return {
        tag: "computing",
        newRef: branchRef(),
        newRefHash: hash(),
        oldRef: "main",
        oldRefHash: hash(),
        project: projectRef,
      };
    case "error":
      return {
        tag: "done",
        diff: {
          tag: "error",
          error: cfg.error,
        },
        newRef: branchRef(),
        newRefHash: hash(),
        oldRef: "main",
        oldRefHash: hash(),
        project: projectRef,
      };
  }
}

type NotificationEventKind =
  | "projectContributionCreated"
  | "projectBranchUpdated";

function notificationEventKind(): NotificationEventKind {
  return faker.helpers.arrayElement([
    "projectContributionCreated",
    "projectBranchUpdated",
  ]);
}

function notificationEventPayload(kind?: NotificationEventKind) {
  const kind_ = !!kind ? kind : notificationEventKind();

  switch (kind_) {
    case "projectContributionCreated": {
      const projectRef_ = projectRef();
      const [projectHandle, projectSlug] = projectRef_.split("/");
      const sourceBranchRef_ = branchRef();
      const targetBranchRef_ = branchRef();

      return {
        author: user(),
        contributionId: faker.string.uuid(),
        description: faker.lorem.paragraphs(),
        number: faker.number.int(100),
        status: contributionStatus(),
        title: faker.lorem.sentences(1),
        mergeSourceBranch: {
          branchContributorHandle: null,
          branchContributorUserId: null,
          branchId: faker.string.uuid(),
          branchName: branchSlugOf(sourceBranchRef_),
          branchShortHand: sourceBranchRef_,
        },
        mergeTargetBranch: {
          branchContributorHandle: null,
          branchContributorUserId: null,
          branchId: faker.string.uuid(),
          branchName: branchSlugOf(targetBranchRef_),
          branchShortHand: targetBranchRef_,
        },
        project: {
          projectId: faker.string.uuid(),
          projectOwnerHandle: projectHandle,
          projectOwnerUserId: faker.string.uuid(),
          projectShortHand: projectRef_,
          projectSlug: projectSlug,
        },
      };
    }
    case "projectBranchUpdated": {
      const projectRef_ = projectRef();
      const [projectHandle, projectSlug] = projectRef_.split("/");
      const branchRef_ = branchRef();
      const contributor = user();

      return {
        branch: {
          branchContributorHandle: contributor.handle,
          branchContributorUserId: contributor.userId,
          branchId: faker.string.uuid(),
          branchName: branchSlugOf(branchRef_),
          branchShortHand: branchRef_,
        },
        project: {
          projectId: faker.string.uuid(),
          projectOwnerHandle: projectHandle,
          projectOwnerUserId: faker.string.uuid(),
          projectShortHand: projectRef_,
          projectSlug: projectSlug,
        },
      };
    }
  }
}

function notificationEvent(kind?: NotificationEventKind) {
  const actor = {
    info: user(),
    kind: "user",
  };

  const kind_ = !!kind ? kind : notificationEventKind();

  return {
    actor: actor,
    data: {
      kind: kind_,
      payload: notificationEventPayload(kind_),
    },
    id: faker.string.uuid(),
    occurredAt: faker.date.past(),
    scope: {
      info: user(),
      kind: "user",
    },
  };
}

type NotificationStatus = "unread" | "read" | "archived";

function notificationStatus(): NotificationStatus {
  return faker.helpers.arrayElement(["unread", "read", "archived"]);
}

function notification(kind?: NotificationEventKind) {
  return {
    event: notificationEvent(kind),
    id: faker.string.uuid(),
    status: notificationStatus(),
  };
}

export {
  projectRef,
  project,
  account,
  user,
  userDetails,
  userHandle,
  org,
  contribution,
  contributionTimeline,
  contributionStatusChangeEvent,
  contributionDiff,
  notification,
  notificationEvent,
  notificationEventPayload,
  type ContributionDiffConfig,
};
