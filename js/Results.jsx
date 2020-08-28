import { h } from "preact";


function getPath(obj, path) {
  return path.split(".").reduce((x, f) => x ? x[f] : null, obj);
}


function row(name, obj, path, force=false) {
  const val = getPath(obj, path);
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


export const Report = ({report}) => {
  const img = getPath(report, "data.carImage.previewUrl");
  return (
    <div class="container" style="padding: 2em">
      <a href={report.pdfLink} target="_blank">Скачать полный отчёт</a>
      { img && <img src={img} /> }

      <table class="table is-hoverable is-fullwidth">
        <tbody>
          { row("VIN", report, "data.head.vin") }
          { row("Вес", report, "data.equipment.netWeight.description") }
          { row("Максимальный вес", report, "data.equipment.maxWeight.description") }
          { row("Объём двигателя", report, "data.equipment.volume.description") }
          { row("Мощность", report, "data.equipment.horsepower.description") }
          { row("Пробег", report, "data.events.lastMileageRecord.description") }
          { row("Владельцы", report, "data.events.owners.description") }
          { row("ДТП", report, "data.events.crashes.description") }
        </tbody>
      </table>
    </div>);
}
