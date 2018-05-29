import React from 'react';
import PropTypes from 'prop-types';
import { connect } from 'react-redux';
import { toggleActivated } from '../../api/Actions/dataLoaderActions';

import TabMenuItem from '../Sidebar/TabMenu/TabMenuItem';

import styles from './ToggleDataLoader.scss';

let ToggleDataLoader = (props) => {
  const { activated } = props;

  return (
    <TabMenuItem active={activated} onClick={() => props.toggleActivated(!activated)}>
      <div className={`${styles.label}`}>DATA LOADER</div>
    </TabMenuItem>
  );
};

ToggleDataLoader.propTypes = {
  activated: PropTypes.bool
};

ToggleDataLoader.defaultProps = {
  activated: false
};

const mapStateToProps = state => ({
  activated: state.dataLoader.activated
});

const mapDispatchToProps = dispatch => ({
  toggleActivated: (activated) => {
    dispatch(toggleActivated(activated));
  }
});

ToggleDataLoader = connect(
  mapStateToProps,
  mapDispatchToProps
)(ToggleDataLoader);

export default ToggleDataLoader;
