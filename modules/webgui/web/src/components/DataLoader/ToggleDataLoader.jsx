import React from 'react';
import PropTypes from 'prop-types';
import { connect } from 'react-redux';
import { setActivated } from '../../api/Actions/dataLoaderActions';
import TabMenuItem from '../common/TabMenu/TabMenuItem';
import Label from '../common/Label/Label';

import styles from './ToggleDataLoader.scss';

let ToggleDataLoader = (props) => {
  const { activated } = props;

  return (
    <TabMenuItem active={activated} onClick={() => props.setActivated(!activated)}>
      <div className={`${styles.container}`}>
        <Label size='medium'>DATA LOADER</Label>
      </div>
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

ToggleDataLoader = connect(
  mapStateToProps,
  null
)(ToggleDataLoader);

export default ToggleDataLoader;
