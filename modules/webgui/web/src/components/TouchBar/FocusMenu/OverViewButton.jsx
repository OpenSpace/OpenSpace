import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Icon from '../../common/Icon/Icon';
import SmallLabel from '../../common/SmallLabel/SmallLabel';
import styles from './FocusButton.scss';

class OverViewButton extends Component {
  constructor(props) {
    super(props);
    this.applyOverview = this.applyOverview.bind(this);
  }

  applyOverview() {
    this.props.onApplyOverview();
  }

  render() {
    return (
      <div className={styles.FocusButton} onClick={this.applyOverview} role="button" tabIndex="0">
        <Icon icon="track_changes" className={styles.Icon} />
        <SmallLabel>Overview</SmallLabel>
      </div>
    );
  }
}

OverViewButton.propTypes = {
  onApplyOverview: PropTypes.func.isRequired,
};

export default OverViewButton;
