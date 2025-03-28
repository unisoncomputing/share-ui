function account(handle: string) {
  return {
    avatarUrl: null,
    completedTours: ["welcome-terms"],
    handle: handle.replace("@", ""),
    name: "Test User",
    organizationMemberships: [],
    primaryEmail: "testuser@example.com",
    userId: "test-user-id",
  };
}

function project(projectRef: string) {
  const [handle, slug] = projectRef.split("/");

  return {
    createdAt: "2023-05-25T01:39:01.955533Z",
    defaultBranch: "main",
    isFaved: true,
    latestRelease: "3.33.0",
    numActiveContributions: 1,
    numClosedContributions: 9,
    numClosedTickets: 52,
    numDraftContributions: 0,
    numFavs: 24,
    numMergedContributions: 77,
    numOpenTickets: 4,
    owner: {
      handle: handle,
      name: handle.replace("@", ""),
      type: "organization",
    },
    permissions: [],
    releaseDownloads: [98, 103, 74, 76, 95],
    slug: slug,
    summary: "The unison base library.",
    tags: [],
    updatedAt: "2025-03-24T18:39:32.320941Z",
    visibility: "public",
  };
}

export { project, account };
