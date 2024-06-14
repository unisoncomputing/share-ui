import React from "https://esm.sh/react@18.2.0";
import Colors from "../common/colors.ts";

type SocialContent = {
  title: string;
  description: string;
  imageUrl: string;
};

const MaxLength = {
  at4: 30,
  at3: 45,
  at2point5: 52,
};

const DefaultSocialContent = {
  title: "Unison Share",
  description: "Explore, read docs about, and share Unison libraries",
  imageUrl: "https://share.unison-lang.org/static/unison-share-social.png",
};

async function defaultSocialImage(): Promise<React.Element> {
  return <img src={DefaultSocialContent.imageUrl} width="1200" height="630" />;
}

export { SocialContent, DefaultSocialContent, MaxLength, defaultSocialImage };
