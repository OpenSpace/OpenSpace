import React, { Component } from 'react';
import Icon from '../../../common/Icon/Icon';
import SmallLabel from '../../../common/SmallLabel/SmallLabel';
import Image from '../../../../../../../../sync/url/images/files/instructions.png';

import styles from './../style/UtilitiesButtons.scss';

class HelpButton extends Component {
  constructor(props) {
    super(props);

    this.state = {
      showImage: false,
    };

    this.handleClick = this.handleClick.bind(this);
  }

  // Show image for 10s and then set state to false
  handleClick() {
    this.setState({ showImage: !this.state.showImage });
    setTimeout(() => {
      this.setState({
        showImage: false,
      });
    }, 10000);
  }

  render() {
    return (
      <div
        className={`${styles.UtilitiesButton}
        ${this.state.showImage && styles.active}`}
        onClick={this.handleClick}
        role="button"
        tabIndex="0"
      >
        <Icon icon="help_outline" className={styles.Icon} />
        { this.state.showImage && <img src={Image} className={styles.Instructions} alt={'instructions'} />}
        <SmallLabel>Help</SmallLabel>
      </div>
    );
  }
}

export default HelpButton;
