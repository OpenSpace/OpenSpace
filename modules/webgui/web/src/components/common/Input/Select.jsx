import React from 'react';
import PropTypes from 'prop-types';
import ReactSelect from 'react-select';
import 'react-select/dist/react-select.css';

import styles from './Select.scss';
import { excludeKeys } from '../../../utils/helpers';

const Select = (props) => {
  const { id, label, direction } = props;
  const inheritedProps = excludeKeys(props, 'label direction');
  return (
    <div className={styles.selectgroup}>
      <ReactSelect
        {...inheritedProps}
        id={id}
        className={`${styles.select} ${styles[direction]}`}
      />
      <label htmlFor={id} className={styles.selectlabel}>{ label }</label>
    </div>
  );
};

Select.id = 0;

Select.propTypes = {
  clearable: PropTypes.bool,
  direction: PropTypes.oneOf(['up', 'down']),
  id: PropTypes.string,
  label: PropTypes.string.isRequired,
  options: PropTypes.arrayOf(
    PropTypes.shape({
      value: PropTypes.string,
      label: PropTypes.string,
    })).isRequired,
  searchable: PropTypes.bool,
};

Select.defaultProps = {
  direction: 'down',
  id: `select-${Select.id++}`,
  searchable: false,
  clearable: false,
};

export default Select;
