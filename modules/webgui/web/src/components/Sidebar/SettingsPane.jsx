import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Pane from './Pane';
import LoadingBlocks from '../common/LoadingBlock/LoadingBlocks';
import FilterList from '../common/FilterList/FilterList';
import DataManager from '../../api/DataManager';
import { AllPropertiesKey, AllScreenSpaceRenderablesKey } from '../../api/keys';
import PropertyCollection from './Properties/PropertyCollection';

import styles from './SettingsPane.scss';

class SettingsPane extends Component {
  constructor(props) {
    super(props);
    this.state = { properties: [], screenSpaceRenderables: [], hasData: false };

    this.receiveData = this.receiveData.bind(this);
  }

  componentDidMount() {
    DataManager.getValue(AllPropertiesKey, this.receiveData('properties'));
    DataManager.getValue(AllScreenSpaceRenderablesKey, this.receiveData('screenSpaceRenderables'));
  }

  receiveData(prop) {
    return ({ value }) => this.setState({ [prop]: value, hasData: true });
  }

  render() {
    const properties = this.state.properties.concat(this.state.screenSpaceRenderables)
      .map(prop => Object.assign(prop, { key: prop.name }));

    return (
      <Pane title="Settings" closeCallback={this.props.closeCallback}>
        { !this.state.hasData && (
          <LoadingBlocks className={Pane.styles.loading} />
        )}

        { properties.length > 0 && (
          <FilterList
            data={properties}
            className={styles.list}
            viewComponent={PropertyCollection}
          />
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
