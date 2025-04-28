import { faker } from "@faker-js/faker";

function account(handle: string) {
  return {
    ...user(),
    handle,
    completedTours: ["welcome-terms"],
    isSuperadmin: faker.datatype.boolean(),
    organizationMemberships: [],
    primaryEmail: faker.internet.email(),
  };
}

function user(handle?: string) {
  const firstName = faker.person.firstName();
  const lastName = faker.person.lastName();
  const handle_ = handle ? handle : faker.lorem.slug(1);

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
    avatarUrl: faker.image.avatar(),
    handle: handle_.replace("@", ""),
    name: faker.company.name(),
    orgId: faker.string.uuid(),
    kind: "org",
    permissions: [],
  };
}

function projectRef(handle?: string) {
  const handle_ = handle ? handle : faker.lorem.slug(1);
  const slug = faker.lorem.slug(2);
  return `@${handle_}/${slug}`;
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
    status: "in_review",
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

export {
  projectRef,
  project,
  account,
  user,
  userDetails,
  org,
  contribution,
  contributionTimeline,
  contributionStatusChangeEvent,
};
