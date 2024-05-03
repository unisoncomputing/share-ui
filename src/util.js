function getCookie(name) {
  if (navigator.cookieEnabled) {
    const value = `; ${document.cookie}`;
    const parts = value.split(`; ${name}=`);
    if (parts.length === 2) return parts.pop().split(";").shift();
    else return null;
  } else {
    return null;
  }
}

export { getCookie };
