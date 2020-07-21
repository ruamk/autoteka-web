import { h } from "preact";


function row(name, obj, path, force=false) {
  const val = path.split(".").reduce((x, f) => x ? x[f] : null, obj);
  return (!val && !force)
    ? null
    : (
      <tr>
        <th>{name}</th>
        <td>{val || "−"}</td>
      </tr>
    );
}


export const Preview = ({preview}) =>
  <div class="container" style="padding: 2em">
    <table class="table is-hoverable is-fullwidth">
      <tbody>
        { row("VIN",         preview, "vin") }
        { row("Госномер",    preview, "regNumber") }
        { row("Марка",       preview, "data.brand") }
        { row("Модель",      preview, "data.model") }
        { row("Год выпуска", preview, "data.year") }
      </tbody>
    </table>
  </div>;
