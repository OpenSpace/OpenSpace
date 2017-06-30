import React, { Component } from 'react';
import SmallLabel from '../common/SmallLabel/SmallLabel';
import Icon from '../common/Icon/Icon';

import styles from './OriginPicker.scss';

class OriginPicker extends Component {
  constructor(props) {
    super(props);

    this.state = {
      origin: 'Earth',
    };
  }

  get icon() {
    // return this.state.origin ? null : (<Icon icon="language" styling={['extralarge']} />);
    return (<Icon icon="language" styling={['extralarge']} />);
  }

  get origin() {
    return this.state.origin;
  }

  render() {
    return (
      <div className={styles.OriginPicker}>
        <div>
          { this.icon }
        </div>
        <div>
          <span>{ this.origin }</span>
          <SmallLabel>Origin</SmallLabel>
        </div>
      </div>
  );
  }
}

export default OriginPicker;
