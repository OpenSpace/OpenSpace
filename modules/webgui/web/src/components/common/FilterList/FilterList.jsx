import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Input from '../Input/Input';

class FilterList extends Component {
  constructor(props) {
    super(props);

    this.state = {
      search: '',
    };

    this.changeSearch = this.changeSearch.bind(this);
  }

  changeSearch(event) {
    this.setState({ search: event.currentTarget.value });
  }

  get filtered() {
    return this.props.data.filter(this.props.matcher);
  }

  render() {
    const EntryComponent = this.props.viewComponent;
    const { search } = this.state;
    return (
      <section className={this.props.className}>
        <Input value={search} placeholder="Search..." onChange={this.changeSearch} clearable />

        <ul>
          { this.filtered.map(entry => (<EntryComponent data={entry} key={entry.key} />)) }
        </ul>
      </section>
    );
  }
}

FilterList.propTypes = {
  className: PropTypes.string,
  data: PropTypes.array.isRequired,
  matcher: PropTypes.func,
  viewComponent: PropTypes.element,
};

FilterList.defaultProps = {
  className: '',
  matcher: () => true,
  viewComponent: props => (<li>{ JSON.stringify(props) }</li>),
};

export default FilterList;
