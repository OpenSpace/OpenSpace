import React from 'react';
import PropTypes from 'prop-types';
import Icon from '../Icon/Icon';
import styles from './InfoBox.scss';

// There is a problem that the small popup wont overflow the containing parents.
// A solution would be to use proper elements with a static position or elements
// with an arbitrary parent, instead of the pseudo elements used here.
const InfoBox = (props) => {
  const { icon, text } = props;
  return (
    <span className={styles.infobox} data-title={text}>
      <Icon icon={icon} />
    </span>
  );
};

InfoBox.propTypes = {
  icon: PropTypes.string,
  text: PropTypes.string.isRequired,
};

InfoBox.defaultProps = {
  icon: 'info',
};

export default InfoBox;
