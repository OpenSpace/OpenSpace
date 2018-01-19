import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Pane from './Pane';
import LoadingBlocks from '../common/LoadingBlock/LoadingBlocks';
import FilterList from '../common/FilterList/FilterList';
import DataManager from '../../api/DataManager';
import PropertyOwner from './Properties/PropertyOwner';
import { connect } from 'react-redux';

import styles from './SettingsPane.scss';

class SettingsPane extends Component {
  constructor(props) {
    super(props);
    this.state = { properties: [], screenSpaceRenderables: [], hasData: false };

  }

  render() {
    const properties = this.props.properties.concat(this.state.screenSpaceRenderables)
      .map(prop => Object.assign(prop, { key: prop.name }));

    return (
      <Pane title="Settings" closeCallback={this.props.closeCallback}>
        { (properties.length == 0 ) && (
          <LoadingBlocks className={Pane.styles.loading} />
        )}

        {( properties.length > 0 ) && (
          <FilterList
            data={properties}
            className={styles.list}
            viewComponent={PropertyOwner}
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

const mapStateToProps = (state) => {
    return {
        properties: state.sceneGraph
    }
};

SettingsPane = connect(
  mapStateToProps,
  )(SettingsPane)

export default SettingsPane;
