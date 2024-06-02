function truncate(maxLength: number, str: string): string {
  if (str.length > maxLength) {
    return str.slice(0, maxLength - 3) + "...";
  } else {
    return str;
  }
}

export { truncate };
