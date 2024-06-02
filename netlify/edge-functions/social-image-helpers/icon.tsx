// TODO this should live in ui-core, and be generated
import React from "https://esm.sh/react@18.2.0";

type IconType = "pencil-ruler";

const pencilRuler = [
  <path d="M10.4714 1.98252C10.8619 1.592 11.4951 1.592 11.8856 1.98252L12.1678 2.26473C12.5583 2.65525 12.5583 3.28841 12.1678 3.67894L10.3303 5.51646L8.63388 3.82004L10.4714 1.98252Z"></path>,
  <path d="M3.19108 0.780777C3.38634 0.585514 3.70292 0.585514 3.89819 0.780777L12.9455 9.82808C13.1407 10.0233 13.1407 10.3399 12.9455 10.5352L11.108 12.3727C10.9127 12.568 10.5961 12.568 10.4009 12.3727L9.4821 11.4539L11.1785 9.75753L10.3303 8.90932L8.63389 10.6057L7.36157 9.33342L9.05799 7.637L8.20978 6.78879L6.51336 8.48521L5.24105 7.2129L6.93747 5.51648L6.08926 4.66827L4.39284 6.36469L3.12053 5.09237L4.81695 3.39596L3.96874 2.54775L2.27232 4.24417L1.35356 3.3254C1.15829 3.13014 1.15829 2.81356 1.35356 2.6183L3.19108 0.780777Z"></path>,
  <path d="M1.84821 12.3021L5.24105 10.6057L6.08926 9.75752L4.39284 8.0611L3.54463 8.90931L1.84821 12.3021Z"></path>,
];

type IconProps = {
  size: number;
  icon: IconType;
  color: string;
};

function Icon(props: IconProps): React.Element {
  const icon = pencilRuler;

  return (
    <svg
      fill={props.color}
      width={props.size}
      height={props.size}
      viewBox="0 0 14 14"
    >
      {icon}
    </svg>
  );
}

export default Icon;
export { IconType, Icon };
