import React, { Component } from 'react';
import PropTypes from 'prop-types';

import styles from './Select.scss';
import { excludeKeys } from '../../../utils/helpers';

class Select extends Component {
  render() {
    const { children, id, label } = this.props;
    const inheritedProps = excludeKeys(this.props, 'label children');
    return (
      <div className={styles.selectgroup}>
        <select {...inheritedProps} className={styles.select}>
          { children }
        </select>
        <label htmlFor={id} className={styles.selectlabel}>{ label }</label>
      </div>
    );
  }
}

Select.id = 0;

Select.propTypes = {
  children: PropTypes.node.isRequired,
  id: PropTypes.string,
  label: PropTypes.string.isRequired,
};

Select.defaultProps = {
  id: `select-${Select.id++}`,
};

export default Select;
