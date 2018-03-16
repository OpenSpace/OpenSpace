import React, { Component } from 'react';
import Icon from '../../common/Icon/Icon';
import SmallLabel from '../../common/SmallLabel/SmallLabel';
import styles from './FocusButton.scss';

class OverViewButton extends Component {
  render() {
    return (
      <div className={`${styles.FocusButton} ${this.isActive && styles.active}`} onClick={this.select} role="button" tabIndex="0">
        <Icon icon="track_changes" className={styles.Icon} />
        <SmallLabel>Overview</SmallLabel>
      </div>
    );
  }
}

export default OverViewButton;
