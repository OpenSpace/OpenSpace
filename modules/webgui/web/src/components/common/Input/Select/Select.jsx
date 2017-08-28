import React from 'react';
import PropTypes from 'prop-types';
import ReactSelect from 'react-select';
import 'react-select/dist/react-select.css';
import Input from '../Input/Input';
import styles from './Select.scss';
import { excludeKeys } from '../../../../utils/helpers';

const Select = (props) => {
  const { label, direction } = props;
  const inheritedProps = excludeKeys(props, 'label direction');
  const id = props.id || `select-${Input.nextId}`;
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

Select.propTypes = {
  clearable: PropTypes.bool,
  direction: PropTypes.oneOf(['up', 'down']),
  id: PropTypes.string,
  label: PropTypes.node.isRequired,
  options: PropTypes.arrayOf(
    PropTypes.shape({
      value: PropTypes.string,
      label: PropTypes.string,
    })).isRequired,
  searchable: PropTypes.bool,
};

Select.defaultProps = {
  direction: 'down',
  id: null,
  searchable: false,
  clearable: false,
};

export default Select;
