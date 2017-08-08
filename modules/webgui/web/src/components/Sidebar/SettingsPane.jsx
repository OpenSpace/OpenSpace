import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Pane from './Pane';
import LoadingString from '../common/LoadingString/LoadingString';
import FilterList from '../common/FilterList/FilterList';
import DataManager from '../../api/DataManager';

import styles from './SettingsPane.scss';

const PROPERTIES_KEY = '__allProperties';

class SettingsPane extends Component {
  constructor(props) {
    super(props);
    this.state = { properties: [], hasData: false };

    this.receiveData = this.receiveData.bind(this);
  }

  componentDidMount() {
    // subscribe to data
    DataManager.getValue(PROPERTIES_KEY, this.receiveData);
  }

  receiveData({ value }) {
    this.setState({ properties: value, hasData: true });
  }

  get properties() {
    return this.state.properties
      .map(prop => Object.assign(prop, { key: prop.Description.Identifier }));
  }

  render() {
    const { properties } = this;

    return (
      <Pane title="Settings" closeCallback={this.props.closeCallback}>
        { !this.state.hasData && (
          <LoadingString loading>
            Loading...
          </LoadingString>
        )}

        { properties.length > 0 && (
          <FilterList data={properties} className={styles.list} />
        )}
      </Pane>
    );
  }
}

SettingsPane.propTypes = {
  closeCallback: PropTypes.func,
};

SettingsPane.defaultProps = {
  closeCallback: null,
};

export default SettingsPane;
