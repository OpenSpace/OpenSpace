import React, { Component } from 'react';
import Icon from '../../common/Icon/Icon';
import SmallLabel from '../../common/SmallLabel/SmallLabel';

import styles from './UtilitiesButtons.scss';

class HelpButton extends Component {
  render() {
    return (
      <div className={`${styles.UtilitiesButton} ${this.isActive && styles.active}`} onClick={this.handleClick} role="button" tabIndex="0">
        <Icon icon="help_outline" className={styles.Icon} />
        <SmallLabel>Help</SmallLabel>
      </div>
    );
  }
}

export default HelpButton;
