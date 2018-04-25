import React, { Component } from 'react';
import Icon from '../../common/Icon/Icon';
import SmallLabel from '../../common/SmallLabel/SmallLabel';
import styles from './UtilitiesButtons.scss';

class HomeButton extends Component {
  render() {
    return (
      <div className={`${styles.UtilitiesButton} ${this.isActive && styles.active}`} onClick={this.select} role="button" tabIndex="0">
        <Icon icon="home" className={styles.Icon} />
        <SmallLabel>Home</SmallLabel>
      </div>
    );
  }
}

export default HomeButton;
