class SupportChatWidget extends HTMLElement {
  constructor() {
    super();
  }

  connectedCallback() {
    const name = this.getAttribute("name");
    const handle = this.getAttribute("handle");
    const avatarUrl = this.getAttribute("avatar-url");

    let avatar;
    if (avatarUrl) {
      avatar = {
        type: "avatar",
        image_url: avatarUrl,
      };
    }

    this.loadIntercom(() => {
      window.Intercom("boot", {
        api_base: "https://api-iam.intercom.io",
        app_id: "c4a188cp",
        name: name,
        user_id: handle,
        avatar: avatar,
      });

      window.Intercom("showMessages");
    });
  }

  loadIntercom(onLoad) {
    let s = document.createElement("script");
    s.type = "text/javascript";
    s.async = true;
    s.src = "https://widget.intercom.io/widget/c4a188cp";
    s.addEventListener("load", onLoad);
    document.body.appendChild(s);
  }
}

customElements.define("support-chat-widget", SupportChatWidget);
