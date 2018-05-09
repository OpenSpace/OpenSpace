import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Icon from '../../common/Icon/Icon';
import SmallLabel from '../../common/SmallLabel/SmallLabel';
import styles from './FocusButton.scss';

// Import all files from a given directory path
function importAll(r) {
  const images = {};
  r.keys().map((item) => { images[item.replace('./', '')] = r(item); });
  return images;
}

// Import all icon images from the given directory
const icons = importAll(require.context('../../../../../../../sync/url/planet_icons/files', false, /\.(png|jpe?g|svg)$/));

class FocusButton extends Component {
  constructor(props) {
    super(props);
    this.select = this.select.bind(this);
  }

  get isActive() {
    return this.props.identifier === this.props.active;
  }

  get icon() {
    const icon = icons[`${this.props.identifier.toLowerCase()}.png`];
    if (icon) {
      return (
        <img src={icon} className={styles.iconImage} alt={this.props.identifier} />
      );
    }
    return (<Icon icon="language" className={styles.Icon} />);
  }

  select() {
    this.isActive ? this.props.onRefocus(this.props.descriptionFlyTo) : this.props.onChangeOrigin(this.props.identifier);
  }

  render() {
    return (
      <div className={`${styles.FocusButton} ${this.isActive && styles.active}`} onClick={this.select} role="button" tabIndex="0">
        { this.icon }
        <SmallLabel>{this.props.identifier}</SmallLabel>
      </div>
    );
  }
}

FocusButton.propTypes = {
  active: PropTypes.string.isRequired,
  identifier: PropTypes.string.isRequired,
  onChangeOrigin: PropTypes.func.isRequired,
  onRefocus: PropTypes.func.isRequired,
};

export default FocusButton;
