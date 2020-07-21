import { h, Component } from "preact";
import { Message, ok, info, err } from "./Message";


export class Search extends Component {
  constructor(props) {
    super(props);
    this.state = {
      query: "",
      message: null
    };
    this.getPreview = this.getPreview.bind(this);
  }


  componentDidMount() {
    document.getElementById("query").focus();
  }


  getPreview(e) {
    e.preventDefault();
    this.setState({message: info("Отправляем запрос в Автотеку...")});

    fetch(`/autoteka/reg/${encodeURIComponent(this.state.query)}`)
      .then(res => res.json())
      .then(res => this.setState(
        {message: null},
        () => this.props.onPreview(res)
      ))
      .catch(e => this.setState({
        message: err("Ничего не найдено. Возможно это какая-то ошибка.")
      }));
  }


  render() {
    const {query, message} = this.state;
    const canSearch = query && query.length > 4;
    return (
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
      </form>);
  }
}
