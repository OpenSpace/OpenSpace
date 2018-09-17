import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import Pane from './Pane';
import LoadingBlocks from '../common/LoadingBlock/LoadingBlocks';
import FilterList from '../common/FilterList/FilterList';
import PropertyOwner from './Properties/PropertyOwner';

import styles from './SettingsPane.scss';

class SettingsPane extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    const { properties } = this.props;

    return (
      <Pane title="Settings" closeCallback={this.props.closeCallback}>
        { (properties.length === 0) && (
          <LoadingBlocks className={Pane.styles.loading} />
        )}

        {(properties.length > 0) && (
          <FilterList
            data={properties}
            className={styles.list}
            viewComponent={PropertyOwner}
            searchAutoFocus
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
  const sceneType = 'Scene';
  const properties = state.propertyTree.subowners.filter(element => element.identifier !== sceneType);
  return {
    properties,
  };
};

SettingsPane = connect(
  mapStateToProps,
)(SettingsPane);

export default SettingsPane;
