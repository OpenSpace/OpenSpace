import React, { Component } from 'react';
import Icon from '../../common/Icon/Icon';
import SmallLabel from '../../common/SmallLabel/SmallLabel';
import styles from './UtilitiesButtons.scss';

class ResetButton extends Component {
  render() {
    return (
      <div className={`${styles.UtilitiesButton} ${this.isActive && styles.active}`} onClick={this.select} role="button" tabIndex="0">
        <Icon icon="replay" className={styles.Icon} />
        <SmallLabel>Reset</SmallLabel>
      </div>
    );
  }
}

export default ResetButton;
