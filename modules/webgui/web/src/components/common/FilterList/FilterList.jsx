import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Input from '../Input/Input/Input';
import CenteredLabel from '../CenteredLabel/CenteredLabel';
import ScrollOverlay from '../ScrollOverlay/ScrollOverlay';
import { SimpleSubstring } from '../../../utils/StringMatchers';
import styles from './FilterList.scss';

class FilterList extends Component {
  constructor(props) {
    super(props);

    this.state = {
      search: '',
    };

    this.changeSearch = this.changeSearch.bind(this);
  }

  changeSearch({ currentTarget }) {
    this.setState({ search: currentTarget.value });
  }

  get filtered() {
    const { favorites, data, matcher } = this.props;
    let { search } = this.state;
    if (search === '') {
      return favorites || data;
    }

    // most matcher functions are case sensitive
    search = search.toLowerCase();

    const matcherFunc = (testObj) => {
      const valuesAsStrings = Object.values(testObj)
        .filter(test => ['number', 'string'].includes(typeof test))
        .map(test => test.toString())
        .map(test => test.toLowerCase());
      return valuesAsStrings.some(test => matcher(test, search));
    };
    return data.filter(matcherFunc);
  }

  render() {
    const EntryComponent = this.props.viewComponent;
    const { search } = this.state;
    const entries = this.filtered;
    return (
      <section className={`${this.props.className} ${styles.filterList}`}>
        <Input
          value={search}
          placeholder={this.props.searchText}
          onChange={this.changeSearch}
          clearable
          autoFocus={this.props.searchAutoFocus}
        />

        <ScrollOverlay>
          { entries.length === 0 && (
            <CenteredLabel>
              Nothing found. Try another search!
            </CenteredLabel>
          ) }
          <ul>
            { entries.map(entry => (
              <EntryComponent
                {...entry}
                key={entry.identifier}
                onClick={this.props.onSelect}
                active={this.props.active}
              />)) }
          </ul>
        </ScrollOverlay>
      </section>
    );
  }
}

FilterList.propTypes = {
  /**
   * the currently active entry, if any. Should be compared strict in viewComponent
   */
  // eslint-disable-next-line react/forbid-prop-types
  active: PropTypes.any,
  /**
   * Class name to apply to the list
   */
  className: PropTypes.string,
  /**
   * the data to display
   */
  // eslint-disable-next-line react/forbid-prop-types
  data: PropTypes.array.isRequired,
  /**
   * Optional: data to display when there is no search term
   * Defaults to the same as `data`
   */
  favorites: PropTypes.array,
  /**
   * the function used to filter the list
   */
  matcher: PropTypes.func,
  /**
   * callback method for selecting an option
   */
  onSelect: PropTypes.func,
  /**
   * Placeholder and label text for the search box
   */
  searchText: PropTypes.string,
  /**
   * Whether the search input field should gain focus automatically
   */
   searchAutoFocus: PropTypes.bool,
  /**
   * the component used to display entries
   */
  viewComponent: PropTypes.func,
};

FilterList.defaultProps = {
  active: null,
  className: '',
  matcher: SimpleSubstring,
  onSelect: null,
  searchText: 'Search...',
  searchAutoFocus: false,
  viewComponent: props => (<li>{ JSON.stringify(props) }</li>),
};

export default FilterList;
