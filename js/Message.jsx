import { h } from "preact";

export const Message = ({color, icon, text}) =>
  <p class={`help is-${color}`}>
    <span class="icon is-small is-right">
      <i class={`fas ${icon}`}></i>
    </span>
    <span>&nbsp;</span>
    <span>{text}</span>
  </p>;

export const ok = text =>
  ({text, color: "success", icon: "fa-check"});

export const info = text =>
  ({text, color: "info", icon: "fa-spinner fa-pulse"});

export const err = text =>
  ({text, color: "danger", icon: "fa-exclamation-triangle"});
