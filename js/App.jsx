
import { h, Component } from "preact";
import cls from "classnames";

import { Tabs, Tab } from "./Tabs";
import { Config, ConfigForm } from "./Config";



const Message = ({color, icon, text}) =>
  <p class={`help is-${color}`}>
    <span class="icon is-small is-right">
      <i class={`fas ${icon}`}></i>
    </span>
    <span>&nbsp;</span>
    <span>{text}</span>
  </p>;

const ok = text => ({text, color: "success", icon: "fa-check"});
const info = text => ({text, color: "info", icon: "fa-spinner fa-pulse"});
const err = text => ({text, color: "danger", icon: "fa-exclamation-triangle"});


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

const Preview = ({preview}) =>
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


export class App extends Component {
  constructor() {
    super();
    this.state = {
      query: "",
      message: null,
      preview: {vin: "12312", regNumber: "54525"},
      report: null
    };
    this.getPreview = this.getPreview.bind(this);
    this.getReport = this.getReport.bind(this);
  }

  componentDidMount() {
    document.getElementById("query").focus();
  }

  getPreview(e) {
    e.preventDefault();
    this.setState({message: info("Отправляем запрос в Автотеку...")});

    fetch(`/autoteka/reg/${encodeURIComponent(this.state.query)}`)
      .then(res => res.json())
      .then(res => this.setState({
        message: null,
        preview: res
      }))
      .catch(e => this.setState({
        message: err("Ничего не найдено. Возможно это какая-то ошибка.")
      }));
  }

  getReport(e) {
  }


  // TODO: расширенный поиск если status != "notFound"
  render() {
    const {query, message, preview, report} = this.state;
    const canSearch = query && query.length > 4;

    const mainVerticalCls = cls({vcentered: !preview});
    return (
      <div class={"main columns is-centered " + mainVerticalCls}>
        <div class="column is-half">
          <form onSubmit={canSearch ? this.getPreview : e => e.preventDefault() }>
            <div class="field is-grouped">
              <div class="control is-expanded">
                <input
                  id="query"
                  class="input"
                  type="text"
                  placeholder="VIN или госномер"
                  value={query}
                  onInput={e => this.setState({query: e.target.value})}/>
                { message && <Message {...message} /> }
              </div>
              <div class="control">
                <a class="button is-info"
                  disabled={!canSearch}
                  onClick={this.getPreview}
                >
                  Поиск
                </a>
              </div>
            </div>
          </form>

          { preview && <Preview preview={preview} /> }
          { !report &&
            <div class="field is-grouped">
              <div class="control">
                <div class="tags has-addons">
                  <span class="tag">Доступно</span>
                  <span class="tag is-primary">10</span>
                </div>
              </div>
              <div class="control">
                <a class="is-small" onClick={this.getReport}>
                  Расширенный поиск
                </a>
              </div>
            </div>
          }
        </div>
      </div>);
  }
}
